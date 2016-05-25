/*
 * File Name    : sinosims-fop.c
 * Author       : Tanhuacheng @ sinosims
 * Version      : V0.0.1
 * Data         : Apr 6, 2015
 * Description  : File operate of USB driver 
*/

#include "sinosims-inc.h"

extern bool debug_info_en;
extern struct usb_driver sinosims_driver;
extern void sinosims_draw_down(struct usb_sinosims *dev);

void sinosims_delete(struct kref *kref)
{
    struct usb_sinosims *dev = to_sinosims_dev(kref);
    
    usb_free_urb(dev->bulk_in_urb);
    usb_put_dev(dev->udev);
    kfree(dev->bulk_in_buffer);
    kfree(dev);
}

static int sinosims_open(struct inode *inode, struct file *file)
{
    struct usb_sinosims *dev;
    struct usb_interface *interface;
    int subminor;
    int retval = 0;

    subminor = iminor(inode);

    interface = usb_find_interface(&sinosims_driver, subminor);
    if (!interface) {
        pr_err("%s - error, can't find device for minor %d\n",
            __func__, subminor);
        retval = -ENODEV;
        goto exit;
    }

    dev = usb_get_intfdata(interface);
    if (!dev) {
        retval = -ENODEV;
        goto exit;
    }

    retval = usb_autopm_get_interface(interface);
    if (retval)
        goto exit;

    /* increment our usage count for the device */
    kref_get(&dev->kref);

    /* save our object in the file's private structure */
    file->private_data = dev;

exit:
    return retval;
}

static int sinosims_release(struct inode *inode, struct file *file)
{
    struct usb_sinosims *dev;

    dev = file->private_data;
    if (dev == NULL)
        return -ENODEV;

    /* allow the device to be autosuspended */
    mutex_lock(&dev->io_mutex);
    if (dev->interface)
        usb_autopm_put_interface(dev->interface);
    mutex_unlock(&dev->io_mutex);

    /* decrement the count on our device */
    kref_put(&dev->kref, sinosims_delete);
    return 0;
}

static int sinosims_flush(struct file *file, fl_owner_t id)
{
    struct usb_sinosims *dev;
    int res;

    dev = file->private_data;
    if (dev == NULL)
        return -ENODEV;

    /* wait for io to stop */
    mutex_lock(&dev->io_mutex);
    sinosims_draw_down(dev);

    /* read out errors, leave subsequent opens a clean slate */
    spin_lock_irq(&dev->err_lock);
    res = dev->errors ? (dev->errors == -EPIPE ? -EPIPE : -EIO) : 0;
    dev->errors = 0;
    spin_unlock_irq(&dev->err_lock);

    mutex_unlock(&dev->io_mutex);

    return res;
}

static void sinosims_read_bulk_callback(struct urb *urb)
{
    struct usb_sinosims *dev;

    dev = urb->context;

    spin_lock(&dev->err_lock);
    /* sync/async unlink faults aren't errors */
    if (urb->status) {
        if (!(urb->status == -ENOENT ||
            urb->status == -ECONNRESET ||
            urb->status == -ESHUTDOWN))
            dev_err(&dev->interface->dev,
                "%s - nonzero write bulk status received: %d\n",
                __func__, urb->status);

        dev->errors = urb->status;
    } else {
        dev->bulk_in_filled = urb->actual_length;
    }
    dev->ongoing_read = 0;
    spin_unlock(&dev->err_lock);

    wake_up_interruptible(&dev->bulk_in_wait);
}

static int sinosims_do_read_io(struct usb_sinosims *dev, size_t count)
{
    int rv;

    /* prepare a read */
    usb_fill_bulk_urb(dev->bulk_in_urb,
            dev->udev,
            usb_rcvbulkpipe(dev->udev,
                dev->bulk_in_endpointAddr),
            dev->bulk_in_buffer,
            min(dev->bulk_in_size, count),
            sinosims_read_bulk_callback,
            dev);
    /* tell everybody to leave the URB alone */
    spin_lock_irq(&dev->err_lock);
    dev->ongoing_read = 1;
    spin_unlock_irq(&dev->err_lock);

    /* submit bulk in urb, which means no data to deliver */
    dev->bulk_in_filled = 0;
    dev->bulk_in_copied = 0;

    /* do it */
    rv = usb_submit_urb(dev->bulk_in_urb, GFP_KERNEL);
    if (rv < 0) {
        dev_err(&dev->interface->dev,
            "%s - failed submitting read urb, error %d\n",
            __func__, rv);
        rv = (rv == -ENOMEM) ? rv : -EIO;
        spin_lock_irq(&dev->err_lock);
        dev->ongoing_read = 0;
        spin_unlock_irq(&dev->err_lock);
    }

    return rv;
}

static ssize_t sinosims_read(struct file *file, char *buffer, size_t count, loff_t *ppos)
{
    struct usb_sinosims *dev;
    int rv;
    bool ongoing_io;

    dev = file->private_data;

    /* if we cannot read at all, return EOF */
    if (!dev->bulk_in_urb || !count)
        return 0;

    /* no concurrent readers */
    rv = mutex_lock_interruptible(&dev->io_mutex);
    if (rv < 0)
        return rv;

    if (!dev->interface) {      /* disconnect() was called */
        rv = -ENODEV;
        goto exit;
    }

    /* if IO is under way, we must not touch things */
retry:
    spin_lock_irq(&dev->err_lock);
    ongoing_io = dev->ongoing_read;
    spin_unlock_irq(&dev->err_lock);

    if (ongoing_io) {
        /* nonblocking IO shall not wait */
        if (file->f_flags & O_NONBLOCK) {
            rv = -EAGAIN;
            goto exit;
        }
        /* IO may take forever, hence wait in an interruptible stae */
        rv = wait_event_interruptible(dev->bulk_in_wait, (!dev->ongoing_read));
        if (rv < 0)
            goto exit;
    }

    /* errors must be reported */
    rv = dev->errors;
    if (rv < 0) {
        /* any error is reported once */
        dev->errors = 0;
        /* to preserve notifications about reset */
        rv = (rv == -EPIPE) ? rv : -EIO;
        /* report it */
        goto exit;
    }

    /* if the buffer is filled we may satisfy the read, else we need to start IO */
    
    if (dev->bulk_in_filled) {
        /* we had read data */
        size_t available = dev->bulk_in_filled - dev->bulk_in_copied;
        size_t chunk = min(available, count);

        if (!available) {
            /* all data has been used, actual IO needs to be done */
            rv = sinosims_do_read_io(dev, count);
            if (rv < 0)
                goto exit;
            else 
                goto retry;
        }
        /* data is available, chunk tells us how much shall be copied */
        if (copy_to_user(buffer, dev->bulk_in_buffer + dev->bulk_in_copied, chunk))
            rv = -EFAULT;
        else 
            rv = chunk;

        dev->bulk_in_copied += chunk;
    
        /* if we are asked for more than we have, we start IO but don't wait */
        if (available < count)
            sinosims_do_read_io(dev, count - chunk);
    } else {
        /* no data in the buffer */
        rv = sinosims_do_read_io(dev, count);
        if (rv < 0)
            goto exit;
        else if (!(file->f_flags & O_NONBLOCK))
            goto retry;
        rv = -EAGAIN;
    }
exit:
    mutex_unlock(&dev->io_mutex);
    return rv;
}

static void sinosims_write_bulk_callback(struct urb *urb)
{
    struct usb_sinosims *dev;

    dev = urb->context;    

    /* sync/async unlink faults aren't errors */
    if (urb->status) {  
        if (!(urb->status == -ENOENT ||
            urb->status == -ECONNRESET ||
            urb->status == -ESHUTDOWN)) {
            dev_err(&dev->interface->dev, "%s - nonzero write bulk status received: %d\n", __func__, urb->status);
        }
        spin_lock(&dev->err_lock);
        dev->errors = urb->status;
        spin_unlock(&dev->err_lock);
    }

    /* free up our allocated buffer */
    usb_free_coherent(urb->dev, urb->transfer_buffer_length, urb->transfer_buffer, urb->transfer_dma);
    up(&dev->limit_sem);
}

static ssize_t sinosims_write(struct file *file, const char *user_buffer, size_t count, loff_t *ppos)
{
    struct usb_sinosims *dev;
    int retval = 0;
    struct urb *urb = NULL;
    char *buf = NULL;
    size_t writesize = min(count, (size_t)MAX_TRANSFER);

    dev = file->private_data;

    /* verify that we actually have some data to write */
    if (count == 0)
        goto exit;

    /* limit the number of URBs in flight to stop a user from using up all RAM */
    if (!(file->f_flags & O_NONBLOCK)) {
        if (down_interruptible(&dev->limit_sem)) {
            retval = -ERESTARTSYS;
            goto exit;
        }
    } else {
        if (down_trylock(&dev->limit_sem)) {
            retval = -EAGAIN;
            goto exit;
        }
   }

    spin_lock_irq(&dev->err_lock);
    retval = dev->errors;
    if (retval < 0) {
        /* any error is reported once */
        dev->errors = 0;
        /* to preserve notifications about reset */
        retval = (retval == -EPIPE) ? retval : -EIO;
    }
    spin_unlock_irq(&dev->err_lock);
    if (retval < 0)
        goto error;

    /* create a urb, and a buffer for it, and copy the data to the urb */
    urb = usb_alloc_urb(0, GFP_KERNEL);
    if (!urb) { 
        retval = -ENOMEM;
        goto error;
    }

    buf = usb_alloc_coherent(dev->udev, writesize, GFP_KERNEL, &urb->transfer_dma);
    if (!buf) {
        retval = -ENOMEM;
        goto error;
    }

    if (copy_from_user(buf, user_buffer, writesize)) {
        retval = -EFAULT;
        goto error;
    }

    /* this lock makes sure we don't submit URBs to gone devices */
    mutex_lock(&dev->io_mutex);
    if (!dev->interface) {      /* disconnect() was called */
        mutex_unlock(&dev->io_mutex); 
        retval = -ENODEV;
        goto error;
    }
    
    /* initialize the urb properly */
    usb_fill_bulk_urb(urb, dev->udev,
                usb_sndbulkpipe(dev->udev, dev->bulk_out_endpointAddr),
                buf, writesize, sinosims_write_bulk_callback, dev);
    urb->transfer_flags |= URB_NO_TRANSFER_DMA_MAP;
    usb_anchor_urb(urb, &dev->submitted);

    /* send the data out the bulk port */
    retval = usb_submit_urb(urb, GFP_KERNEL);
    mutex_unlock(&dev->io_mutex);
    if (retval) {
        dev_err(&dev->interface->dev, "%s - failed submitting write urb, error %d\n", __func__, retval);
        goto error_unanchor;
    }

    /* release our reference to this urb, the USB core will eventually free it entirely */
    usb_free_urb(urb);

    return writesize;

error_unanchor:
    usb_unanchor_urb(urb);
error:
    if (urb) {
        usb_free_coherent(dev->udev, writesize, buf, urb->transfer_dma);
        usb_free_urb(urb);
    }
    up(&dev->limit_sem);

exit:
    return retval;
}

const struct file_operations sinosims_fops = {
    .owner =    THIS_MODULE,
    .read =     sinosims_read,
    .write =    sinosims_write,
    .open =     sinosims_open,
    .release =  sinosims_release,
    .flush =    sinosims_flush,
    .llseek =   noop_llseek,    /* nop operate, we do not access seek operation */
};

// end of file
