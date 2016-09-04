// test.c

#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>

struct point {
    int x;
    int y;
};

int main (int16_t argc, char* argv[])
{
    struct point p;

    p.x = 1;
    p.y = 1;

    p.x = 2;
    p.y = 2;

    if (1) {
        ;
    }
    ;
    ;	;
    while (1 && 2 &&
           3) {
        break;
    }
    
    printf("hello, world! p{x: %d, y: %d}\n", p.x, p.y);
  
}
