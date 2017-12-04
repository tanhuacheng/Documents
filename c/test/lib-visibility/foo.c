#include <stdio.h>

void dup (void)
{
    printf("dup - foo\n");
}

__attribute((visibility("default"))) void foo (void)
{
    printf("foo: ");
    dup();
}

// end of file
