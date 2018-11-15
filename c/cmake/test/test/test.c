#include <stdio.h>

void test(int argc, char *argv[])
{
    printf("test:");
    for (int i = 0; i < argc; i++) {
        printf(" %s", argv[i]);
    }
    printf("\n");
}
