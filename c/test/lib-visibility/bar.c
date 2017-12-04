#include <stdio.h>

void dup (void)
{
    printf("dup - bar\n");
}

__attribute((visibility("default"))) void bar (void)
{
    printf("bar: ");
    dup();
}

// end of file
