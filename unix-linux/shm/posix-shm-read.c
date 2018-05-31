#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

int main (int argc, char* argv[])
{
    int fd;
    char* addr;
    struct stat sb;

    fd = shm_open("shm-test", O_RDONLY, 0);
    fstat(fd, &sb);
    addr = mmap(NULL, sb.st_size, PROT_READ, MAP_SHARED, fd, 0);
    printf("size = %d\n", (int)sb.st_size);
    for (int i = 0; i < sb.st_size; i++) {
        printf("%02x", addr[i]);
    }
    printf("\n");

    while (1) {
        printf("%.*s\n", (int)sb.st_size, addr);
        sleep(1);
    }

    return 0;
}

