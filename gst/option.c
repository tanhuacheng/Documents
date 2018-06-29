#include <stdio.h>
#include <gst/gst.h>

int main(int argc, char *argv[])
{
    gboolean silent = FALSE;
    gchar *savefile = NULL;

    GOptionEntry entries[] = {
        {"silent", 's', 0, G_OPTION_ARG_NONE, &silent,
         "do not output status information", NULL},
        {"output", 'o', 0, G_OPTION_ARG_STRING, &savefile,
         "save xml representation of pipeline to FILE and exit", "FILE"},
        {0}
    };

    GOptionContext *ctx = g_option_context_new("- Your application");
    /* g_option_context_add_main_entries(ctx, entries, NULL); */
    g_option_context_add_group(ctx, gst_init_get_option_group());

    GError *err = NULL;
    if (!g_option_context_parse(ctx, &argc, &argv, &err)) {
        g_print("Failed to initialize: %s\n", err->message);
        g_clear_error(&err);
        g_option_context_free(ctx);
        return 1;
    }
    g_option_context_free(ctx);

    printf("%d, %s\n", silent, savefile ?: "NULL");

    printf("Run me with --help to see the Application options appended.\n");

    return 0;
}
