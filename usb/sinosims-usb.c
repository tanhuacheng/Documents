/*
 * File Name    : sinosims-usb.c
 * Author       : Tanhuacheng @ sinosims
 * Version      : V0.0.1
 * Data         : Apr 6, 2015
 * Description  : USB driver for sinosims' main engine module
*/

#include "sinosims-inc.h"

/* Define vendor and product identifiers to match our device */ 
#define USB_SINOSIMS_VENDOR_ID  0X03EB
#define USB_SINOSIMS_PRODUCT_ID 0X6133

/* table of devices that work with this driver*/
static struct usb_device_id sinosims_table[] = {
    { USB_DEVICE(USB_SINOSIMS_VENDOR_ID, USB_SINOSIMS_PRODUCT_ID) },
    { }             /* Terminating entry */
};
MODULE_DEVICE_TABLE(usb, sinosims_table);

/* Get a minor range for our devices from the use maintainer */ 
#define USB_SINOSIMS_MINOR_BASE 192

/*
 * usb class driver info in order to get a minor number from the usb core,
 * and to have the device registered with the driver core
*/
static struct usb_class_driver sinosims_class = {
    .name =         "sino%d",
    .fops =         &sinosims_fops,
    .minor_base =   USB_SINOSIMS_MINOR_BASE, 
};

static int sinosims_probe(struct usb_interface *interface,
                const struct usb_device_id *id)
{
    struct usb_sinosims *dev;
    struct usb_host_interface *iface_desc;
    struct usb_endpoint_descriptor *endpoint;
    size_t buffer_size;
    int i;
    int retval = -ENOMEM;

    /* allocate memory for our device stat and initialize it */    
    dev = kzalloc(sizeof(*dev), GFP_KERNEL);
    if (!dev) {
        dev_err(&interface->dev, "Out of memory\n");
        goto error;
    }
    kref_init(&dev->kref);
    sema_init(&dev->limit_sem, WRITES_IN_FLIGHT);
    mutex_init(&dev->io_mutex);
    spin_lock_init(&dev->err_lock);
    init_usb_anchor(&dev->submitted);
    init_waitqueue_head(&dev->bulk_in_wait);

    dev->udev = usb_get_dev(interface_to_usbdev(interface));
    dev->interface = interface;

    /* set up the endpint information */
    /* our device have only one configuration, in this configuration we have three interface.
       for each interface, there are tow endpints(one is bulk-in, and the other one is bulk-out) */ 
    iface_desc = interface->cur_altsetting;
    for (i = 0; i < iface_desc->desc.bNumEndpoints; ++i) {
        endpoint = &iface_desc->endpoint[i].desc;
        
        if (!dev->bulk_in_endpointAddr && 
            usb_endpoint_is_bulk_in(endpoint)) {
            /* we found a bulk in endpoint */
            buffer_size = usb_endpoint_maxp(endpoint);
            dev->bulk_in_size = buffer_size;
            dev->bulk_in_endpointAddr = endpoint->bEndpointAddress;
            dev->bulk_in_buffer = kmalloc(buffer_size, GFP_KERNEL);
            if (!dev->bulk_in_buffer) {
                dev_err(&interface->dev, "Could not allocate bulk_in_buffer\n");
                goto error;
            }
            dev->bulk_in_urb = usb_alloc_urb(0, GFP_KERNEL);
            if (!dev->bulk_in_urb) {
                dev_err(&interface->dev, "Could not allocate bulk_in_urb\n");
                goto error;
            }
        }
        
        if (!dev->bulk_out_endpointAddr && 
            usb_endpoint_is_bulk_out(endpoint)) {
            /* we found a bulk out endpoint */
            dev->bulk_out_endpointAddr = endpoint->bEndpointAddress;
        }
    }
    if (!(dev->bulk_in_endpointAddr && dev->bulk_out_endpointAddr)) {
        dev_err(&interface->dev, "Could not find both bulk-in and bulk-out endpoints\n");
        goto error;
    }
    
    /* save our data pointer in this interface device */
    usb_set_intfdata(interface, dev);
    
    /* we can register the device now, as it is ready */
    retval = usb_register_dev(interface, &sinosims_class);
    if (retval) {
        /* something prevented us from registering this driver */
        dev_err(&interface->dev, "Not albe to get a minor for this device\n");
        usb_set_intfdata(interface, NULL);
        goto error;
    }

    /* let the user know what node this device is now attached to */
    dev_info(&interface->dev, "USB Sinosims device now attached to USBSino-%d", interface->minor);
    return 0;

error:
    if (dev) {
        /* this frees allocated memory */
        kref_put(&dev->kref, sinosims_delete);
    }
    return retval;
}

static void sinosims_disconnect(struct usb_interface *interface)
{
    struct usb_sinosims *dev;
    int minor = interface->minor;

    dev = usb_get_intfdata(interface);
    usb_set_intfdata(interface, NULL);
    
    /* give back our minor */    
    usb_deregister_dev(interface, &sinosims_class);

    /* prevent more I/O from starting */
    mutex_lock(&dev->io_mutex);
    dev->interface = NULL;
    mutex_unlock(&dev->io_mutex);

    usb_kill_anchored_urbs(&dev->submitted);

    /* decrement our usage count */
    kref_put(&dev->kref, sinosims_delete);

    dev_info(&interface->dev, "USB Sinosims #%d now disconnected", minor);
}

static void sinosims_draw_down(struct usb_sinosims *dev)
{
    int time;
    
    time = usb_wait_anchor_empty_timeout(&dev->submitted, 1000);
    if (!time)
        usb_kill_anchored_urbs(&dev->submitted);
    usb_kill_urb(dev->bulk_in_urb);
}

static int sinosims_suspend(struct usb_interface *intf, pm_message_t message)
{
    struct usb_sinosims *dev = usb_get_intfdata(intf);

    if (dev)
        sinosims_draw_down(dev);
    return 0;
}

static int sinosims_resume(struct usb_interface *intf)
{
    return 0;
}

static int sinosims_pre_reset(struct usb_interface *intf)
{
    struct usb_sinosims *dev = usb_get_intfdata(intf);

    mutex_lock(&dev->io_mutex);
    sinosims_draw_down(dev);

    return 0;
}

static int sinosims_post_reset(struct usb_interface *intf)
{
    struct usb_sinosims *dev = usb_get_intfdata(intf);

    /* we are sure no URBs are active - no locking needed */
    dev->errors = -EPIPE;
    mutex_unlock(&dev->io_mutex);

    return 0;
}


static struct usb_driver sinosims_driver = {
    .name =         "sinosims",
    .probe =        sinosims_probe,
    .disconnect =   sinosims_disconnect,
    .suspend =      sinosims_suspend,
    .resume =       sinosims_resume,
    .pre_reset =    sinosims_pre_reset,
    .post_reset =   sinosims_post_reset,
    .id_table =     sinosims_table,
    .supports_autosuspend = 1,
};

/* uncomment the following code if the kernel version is 2.6.x or below
   and comment "module_usb_driver(sinosims_driver) " */
/*
static int __init usb_sinosims_init(void)
{
    int result;

    // register this driver with the USB subsystem 
    result = usb_register(&sinosims_driver);
    if (result)
        err("usb_register failed. Error number %d", result);
    debug_info("usb_sinosims_init.\n");
    return result;
}

static void __exit usb_sinosims_exit(void)
{
    // deregister this driver with the USB subsystem
    usb_deregister(&sinosims_driver);
    debug_info("usb_sinosims_exit.\n");
}

module_init(usb_sinosims_init);
module_exit(usb_sinosims_exit);
*/

/* use module_usb_driver instead of module_init and module_exit to makes
   the code simpler, in addition to the fact currently usb_sinosims_init() 
   ignores usb_deregister() error. */
module_usb_driver(sinosims_driver);

MODULE_LICENSE("GPL");

// selectable module section  declaration
MODULE_AUTHOR("Tanhuacheng @ sinosims");
MODULE_DESCRIPTION("USB driver for sinosims' main engine module");
MODULE_VERSION("V0.0.1");

// end of file
