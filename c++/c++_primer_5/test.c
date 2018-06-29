#include <stdio.h>
#include <stdint.h>

struct point {
    int x, y;
};

void foo(struct point p)
{
    printf("%d,%d\n", p.x, p.y);
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    foo((struct point){1,2});

    return 0;
}
