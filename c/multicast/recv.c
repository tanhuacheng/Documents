#define _BSD_SOURCE
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <arpa/inet.h>
#include <features.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    int fd = socket(AF_INET, SOCK_DGRAM, 0);

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(7080);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    bind(fd, (struct sockaddr*)&addr, sizeof(addr));

    int loop = 1;
    setsockopt(fd, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof(loop));

    struct ip_mreq mreq;
    mreq.imr_multiaddr.s_addr = inet_addr("224.0.0.80");
    mreq.imr_interface.s_addr = htonl(INADDR_ANY);
    setsockopt(fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq));

    while (1) {
        char buf[256] = "";
        ssize_t n = recvfrom(fd, buf, sizeof(buf), 0, (void*)&addr, &(socklen_t){sizeof(addr)});
        if (n > 0) {
            printf("%s: %s\n", inet_ntoa(addr.sin_addr), buf);
        }
    }

    return 0;
}
