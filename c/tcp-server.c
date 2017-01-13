#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>

static void tcp_server (int fd)
{
    char buff[128];

    while (1) {
        int res = read(fd, buff, sizeof(buff));
        if (res < 0) {
            perror("read");
            close(fd);
            return;
        }
        if (!res) {
            printf("connect closed\n");
            close(fd);
            return;
        }
        printf("%.*s\n", res, buff);
    }
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        perror("socket");
        exit(1);
    }

    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_port = htons(8000),
        .sin_addr.s_addr = inet_addr("127.0.0.1"),
    };
    if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        perror("bind");
        exit(1);
    }
    if (listen(fd, 10) < 0) {
        perror("listen");
        exit(1);
    }

    while (1) {
        printf("accept ...\n");

        int cfd = accept(fd, (struct sockaddr*)&addr, &(socklen_t){0});
        if (cfd < 0) {
            perror("accept");
            continue;
        }
        printf("accept %d\n", cfd);

        switch (fork()) {
            case -1: 
                perror("fork");
                close(cfd);
                break;
            case 0:
                close(fd);
                tcp_server(cfd);
                exit(0);
            default:
                close(cfd);
                break;
        }
    }

    return 0;
}
