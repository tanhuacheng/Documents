#include <stdio.h>
#include <sys/socket.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    int fd = socket(AF_INET, SOCK_DGRAM, 0);

    return 0;
}
