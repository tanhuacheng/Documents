#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    if (SIG_ERR == signal(SIGPIPE, SIG_IGN)) {
        perror("signal");
        exit(0);
    }

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

    if (connect(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        perror("connect");
        exit(1);
    }

    char buff[9+4] = {
        0x00, 0x00, 0x00, 0x00, 0xf0, 0xf0, 0x75, 0x03, 0x00, 0x51, 0x00, 0x01, 0x0a
    };

    *(uint32_t*)buff = htonl(9);

    while (1) {
        ssize_t r;
        r = send(fd, buff, sizeof(buff), 0);
        printf("send: %ld\n", r);
        if (r < 0) {
            perror("send");
            exit(1);
        }

        char temp[128] = "";
        r = recv(fd, temp, sizeof(temp), 0);
        printf("recv %ld\n", r);
        if (r < 0) {
            perror("recv");
        }
        if (r == 0) {
            printf("tcp closed\n");
            close(fd);
            break;
        }
        for (int i = 0; i < r; i++) {
            printf("%02x ", temp[i]);
        }
        printf("\n");
        getchar();
    }

    return 0;
}
