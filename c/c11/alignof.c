#include <stdio.h>
#include <stdalign.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

#pragma pack(push, 1)
    struct foo {
        char c1;
        int i;
        char c2;
    };
#pragma pack(pop)

    struct bar {
        char c1;
        int i;
        char c2;
    };

    printf("alignof int: %ld\n", alignof(int));         // 4
    printf("alignof foo: %ld\n", alignof(struct foo));  // 1
    printf("alignof bar: %ld\n", alignof(struct bar));  // 4

    printf("sizeof int: %ld\n", sizeof(int));           // 4
    printf("sizeof foo: %ld\n", sizeof(struct foo));    // 6
    printf("sizeof bar: %ld\n", sizeof(struct bar));    // 12

    return 0;
}
