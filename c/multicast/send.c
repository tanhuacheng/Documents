#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char *argv[])
{
    if (argc != 2) {
        exit(EXIT_SUCCESS);
    }

    int fd = socket(AF_INET, SOCK_DGRAM, 0);

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(7080);
    addr.sin_addr.s_addr = inet_addr("224.0.0.80");

    sendto(fd, argv[1], strlen(argv[1]), 0, (void*)&addr, sizeof(addr));

    return 0;
}
