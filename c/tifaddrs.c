#define __USE_MISC
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    struct ifaddrs* ifaddr;

    if (getifaddrs(&ifaddr) < 0) {
        perror("getifaddrs");
        exit(EXIT_FAILURE);
    }

    for (struct ifaddrs* ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next)  {
        if (NULL == ifa->ifa_addr || AF_INET != ifa->ifa_addr->sa_family ||
            !(ifa->ifa_flags & IFF_RUNNING)) {
            continue;
        }
        printf("%s: %s, %s\n",
            ifa->ifa_name,
            inet_ntoa(((struct sockaddr_in*)ifa->ifa_addr)->sin_addr),
            inet_ntoa(((struct sockaddr_in*)ifa->ifa_netmask)->sin_addr));
    }

    freeifaddrs(ifaddr);

    return 0;
}

