#include <stdlib.h>
#include <syslog.h>
#include <unistd.h>

#define BUF_SIZE (4096)

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    syslog(LOG_INFO, "echo server start %m");

    char buf[BUF_SIZE];
    ssize_t nr;

    while ((nr = read(STDIN_FILENO, buf, sizeof(buf))) > 0) {
        if (write(STDOUT_FILENO, buf, nr) != nr) {
            syslog(LOG_ERR, "write failed: %m");
            exit(1);
        }
    }

    if (nr < 0) {
        syslog(LOG_ERR, "Error from read: %m");
        exit(1);
    }

    syslog(LOG_INFO, "echo server stop");
    close(STDIN_FILENO);
    close(STDOUT_FILENO);

    exit(0);

    return 0;
}
