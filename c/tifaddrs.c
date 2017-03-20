#include <stdio.h>
#include <arpa/inet.h>
#include "guess_localip.h"

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    char straddr[INET_ADDRSTRLEN] = "";
    if (guess_localip(straddr, sizeof(straddr), IFF_MULTICAST)) {
        printf("%s\n", straddr);
    } else {
        printf("Network Unreachable\n");
    }

    return 0;
}

