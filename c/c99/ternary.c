#include <stdio.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    int x;

    x = 1;
    printf("%d\n", x ?: 2);

    x = 0;
    printf("%d\n", x ?: 3);

    return 0;
}
