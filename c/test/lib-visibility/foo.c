#include <stdio.h>

void dup (void)
{
    printf("dup - foo\n");
}

void dym (void);

__attribute((visibility("default"))) void foo (void)
{
    printf("foo: ");
    dup();
    dym();
}

// end of file
