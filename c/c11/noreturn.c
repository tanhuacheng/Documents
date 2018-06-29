#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <unistd.h>

noreturn int foo(void)
{
    while (1) {
        sleep(-1);
    }
}

int bar(void)
{
    foo();
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    bar();
}
