// guess_localip.c

/* 包含头文件 ------------------------------------------------------------------------------------*/
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <ifaddrs.h>
#include "guess_localip.h"

/* 函数定义 --------------------------------------------------------------------------------------*/
const char* guess_localip (char* straddr, size_t addrstrlen, unsigned int flag)
{
    if (!straddr || !addrstrlen) {
        return NULL;
    }

    memset(straddr, '\0', addrstrlen);

    struct ifaddrs* ifaddrs;
    if (getifaddrs(&ifaddrs) < 0) {
        return NULL;
    }

    unsigned long localip = 0;
    for (struct ifaddrs* ifa = ifaddrs; ifa != NULL; ifa = ifa->ifa_next)  {
        if (!ifa->ifa_addr || ifa->ifa_addr->sa_family != AF_INET ||
            !(ifa->ifa_flags & IFF_RUNNING) || ifa->ifa_flags & IFF_LOOPBACK ||
            ((struct sockaddr_in*)ifa->ifa_addr)->sin_addr.s_addr == htonl(INADDR_ANY      ) ||
            ((struct sockaddr_in*)ifa->ifa_addr)->sin_addr.s_addr == htonl(INADDR_LOOPBACK ) ||
            ((struct sockaddr_in*)ifa->ifa_addr)->sin_addr.s_addr == htonl(INADDR_BROADCAST)) {
            continue;
        }

        if ((ifa->ifa_flags & flag) != flag) {
            continue;
        }

        unsigned long s_addr = ((struct sockaddr_in*)ifa->ifa_addr)->sin_addr.s_addr;
        if (!localip) {
            localip = s_addr;
        }

        // 优先选择局域网 IP
        unsigned long ip = ntohl(s_addr);
        if ((ip >= ntohl(inet_addr("192.168.0.0")) && ip <= ntohl(inet_addr("192.168.255.255"))) ||
            (ip >= ntohl(inet_addr("172.16.0.0" )) && ip <= ntohl(inet_addr("172.31.255.255" ))) ||
            (ip >= ntohl(inet_addr("10.0.0.0"   )) && ip <= ntohl(inet_addr("10.255.255.255" )))) {
            localip = s_addr;
            break;
        }
    }
    freeifaddrs(ifaddrs);

    if (!localip) {
        return NULL;
    }

    return inet_ntop(AF_INET, &(struct in_addr){.s_addr = localip}, straddr, addrstrlen);
}

/****************************************** end of file *******************************************/
