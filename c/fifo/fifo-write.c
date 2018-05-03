#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <poll.h>
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    signal(SIGPIPE, SIG_IGN);

    int fd = open("fifo", O_WRONLY | O_NONBLOCK);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    // int flags = fcntl(fd, F_GETFL);
    // flags &= ~O_NONBLOCK;
    // fcntl(fd, F_SETFL, flags);

    while (1) {
        struct pollfd pollfd;
        pollfd.fd = fd;
        pollfd.events = POLLOUT;

        printf("poll: %d\n", poll(&pollfd, 1, -1));
        if (pollfd.revents & POLLERR) {
            printf("error\n");
            return 0;
        }
        if (pollfd.revents & POLLHUP) {
            printf("hup\n");
            return 0;
        }
        if (pollfd.revents & POLLNVAL) {
            printf("nval\n");
            return 0;
        }
        write(fd, "hi", 2);
    }

    return 0;
}
