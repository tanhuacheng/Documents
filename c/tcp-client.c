#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>

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

    if (connect(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        perror("connect");
        exit(1);
    }

    char buff[] = "abcdefghijklmnopqrstuvwxyz1234567890";

    while (1) {
        if (write(fd, buff, sizeof(buff)) < 0) {
            perror("write");
            exit(1);
        }
        sleep(3);
    }

    return 0;
}
