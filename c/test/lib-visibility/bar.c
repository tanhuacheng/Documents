#include <stdio.h>

void dup (void)
{
    printf("dup - bar\n");
}

void dym (void);

__attribute((visibility("default"))) void bar (void)
{
    printf("bar: ");
    dup();
    dym();
}

// end of file
