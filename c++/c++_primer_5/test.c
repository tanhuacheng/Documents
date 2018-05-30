#include <stdio.h>
#include <stdint.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    int i;
    int *p = (int*)1;

    i = (intptr_t)p;
    printf("%d\n", i);

    /* if (((i = 0) = 1)) { */
    /*     printf("the result of assignment is its left-hand operand, which is an lvalue\n"); */
    /* } */

    printf("compile as %s\n", sizeof('a') == 1 ? "c++" : "c");

    char a = 'a';
    printf("       'a': %ld\n", sizeof('a'));
    printf("      char: %ld\n", sizeof(a));
    printf("2147483647: %ld\n", sizeof(2147483647)); // 4
    printf("2147483648: %ld\n", sizeof(2147483648)); // 8
    printf("0x7fffffff: %ld\n", sizeof(0x7fffffff)); // 4
    printf("0xffffffff: %ld\n", sizeof(0xffffffff)); // 4

    return 0;
}
