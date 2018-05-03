#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    int fd = open("fifo", O_RDONLY | O_NONBLOCK);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    // int flags = fcntl(fd, F_GETFL);
    // flags &= ~O_NONBLOCK;
    // fcntl(fd, F_SETFL, flags);

    char buff[128];
    int n;

    while (1) {
        sleep(10);
        close(fd);
        return 0;

        n = read(fd, buff, sizeof(buff));
        if (n < 0) {
            perror("read");
            return 1;
        }
        if (n == 0) {
            // printf("n = 0\n");
        } else {
            printf("%.*s\n", n, buff);
        }
    }

    return 0;
}
