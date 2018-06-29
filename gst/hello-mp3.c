#include <stdio.h>
#include <gst/gst.h>

static gboolean bus_call(GstBus *bus, GstMessage *msg, gpointer data)
{
    (void)bus;
    GMainLoop *loop = data;

    switch (GST_MESSAGE_TYPE(msg)) {
        case GST_MESSAGE_EOS:
            g_print("End of stream\n");
            g_main_loop_quit(loop);
            break;

        case GST_MESSAGE_ERROR: {
            gchar *debug;
            GError *error;

            gst_message_parse_error(msg, &error, &debug);
            g_free(debug);

            g_printerr("Error: %s\n", error->message);
            g_error_free(error);

            g_main_loop_quit(loop);
            break;
        }

        default:
            break;
    }

    return TRUE;
}

int main(int argc, char *argv[])
{
    if (2 != argc) {
        printf("Usage: %s <mp3 filename>\n", argv[0]);
        return -1;
    }

    gst_init(&argc, &argv);
    GMainLoop *loop = g_main_loop_new(NULL, FALSE);

    GstElement *pipeline = gst_pipeline_new("hello-mp3");
    GstElement *source = gst_element_factory_make("filesrc", "file-source");
    GstElement *decoder = gst_element_factory_make("mad", "mp3-decoder");
    GstElement *conv = gst_element_factory_make("audioconvert", "converter");
    GstElement *sink = gst_element_factory_make("autoaudiosink", "audio-sink");

    if (!pipeline || !source || !decoder || !conv || !sink) {
        g_print("One element could not be created. Exiting.\n");
        return -1;
    }

    g_object_set(G_OBJECT(source), "location", argv[1], NULL);

    GstBus *bus = gst_pipeline_get_bus(GST_PIPELINE(pipeline));
    guint bus_watch_id = gst_bus_add_watch(bus, bus_call, loop);
    gst_object_unref(bus);

    gst_bin_add_many(GST_BIN(pipeline), source, decoder, conv, sink, NULL);
    gst_element_link_many(source, decoder, conv, sink, NULL);
    gst_element_set_state(pipeline, GST_STATE_PLAYING);

    g_print("Running ...\n");
    g_main_loop_run(loop);

    g_print("Returned. stopping playback\n");
    gst_element_set_state(pipeline, GST_STATE_NULL);

    g_print("Deleting pipeline\n");
    gst_object_unref(GST_OBJECT(pipeline));
    g_source_remove(bus_watch_id);
    g_main_loop_unref(loop);

    return 0;
}
