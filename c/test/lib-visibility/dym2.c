#include <stdio.h>

void dym (void)
{
    printf("dym2\n");
}

__attribute((visibility("default"))) void dym2 (void)
{
    printf("dym2 - global\n");
}

// end of file
