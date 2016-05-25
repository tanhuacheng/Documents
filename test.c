#include <assert.h>
#include <stdio.h>

int main (int argc, char* argv[])
{
    int i = 1;

    assert(2 == (i = 2));
    printf("i = %d\n", i);

//    assert(1 == (i = 2));
//    printf("i = %d\n", i);

/*
    int *p = NULL, *q = NULL;
    char *x = NULL;

    q++;
    printf("q%p\n", q);

    printf("%p\n", p);
    p++;
    printf("%p\n", p);
    p++;
    printf("%p\n", p);
    p += 1;
    printf("%p\n", p);

    printf("%d\n", p > x);
*/

    int a[3][2];
    int (*p)[2] = a;

    printf("%p,%p\n", a, a + 1);
    printf("%p,%p\n", p[0], p[0] + 1);

    return 0;
}

