#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>

const char* str1 = "tanhuacheng-test";
const char* str2 = "abcdefghijklkjas";

int main (int argc, char* argv[])
{
    /*
     * shm_open
     * ftruncate
     * mmap
     * */
    int fd;
    char* addr;

    fd = shm_open("shm-test", O_RDWR | O_CREAT, 0600);
    ftruncate(fd, 128);
    addr = mmap(NULL, 128, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    close(fd);

    while (1) {
        memcpy(addr, str1, strlen(str1));
        sleep(1);
        memcpy(addr, str2, strlen(str2));
        sleep(1);
    }

    return 0;
}

