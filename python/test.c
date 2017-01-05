#include <stdio.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    printf("%d, %d, %d\n", -1 >> 1, -1 / 2, -1 % 2);

    return 0;
}
