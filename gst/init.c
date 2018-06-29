#include <stdio.h>
#include <gst/gst.h>

int main(int argc, char *argv[])
{
    const gchar *nano_str;
    guint major, minor, micro, nano;

    gst_init(&argc, &argv);

    gst_version(&major, &minor, &micro, &nano);

    if (1 == nano) {
        nano_str = "(CVS)";
    } else if (2 == nano) {
        nano_str = "(Prerelease)";
    } else {
        nano_str = "";
    }

    printf("This program is linked against GStreamer %d.%d.%d %s\n",
           major, minor, micro, nano_str);

    return 0;
}
