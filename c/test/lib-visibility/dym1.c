#include <stdio.h>

void dym (void)
{
    printf("dym1\n");
}

__attribute((visibility("default"))) void dym1 (void)
{
    printf("dym1 - global\n");
}

// end of file
