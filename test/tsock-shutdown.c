#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>                                 
#include <arpa/inet.h>

static int udp_socket_create (const unsigned short lport)
{
    struct sockaddr_in addr;
    int fd;

    fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) { 
        perror("socket");
        return -1;
    }    

    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(lport);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) { 
        perror("bind");
        return -1;
    }    

    return fd;
}

static int fd = -1;

static void* thread_shutdown (void* args)
{
    args = args;

    sleep(1); // 确保已调用 recvfrom
    printf("press %c, shutdown socket\n", getchar());
    shutdown(fd, SHUT_RD);
    return NULL;
}

int main (int argc, char* argv[])
{
    if ((fd = udp_socket_create(8888)) < 0) {
        perror("socket udp 8888");
        exit(0);
    }

    pthread_t tid;
    if (pthread_create(&tid, NULL, thread_shutdown, NULL)) {
        perror("pthread_create");
        exit(0);
    }

    static uint8_t buff[128];
    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    ssize_t result = recvfrom(fd, buff, sizeof(buff), 0, (struct sockaddr*)&addr, &addrlen);
    printf("return value of \"recvfrom\" = %ld\n", result);
    result = recvfrom(fd, buff, sizeof(buff), 0, (struct sockaddr*)&addr, &addrlen);
    printf("return value of \"recvfrom\" = %ld\n", result);

    pthread_join(tid, NULL);
    close(fd);

    return 0;
}

