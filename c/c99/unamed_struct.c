#include <stdio.h>

struct test {
    struct { int x; int y; };
    int z;
};

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    struct test test = {.x = 1, .y = 2, .z = 3};
    printf("%d, %d, %d\n", test.x, test.y, test.z);

    return 0;
}
