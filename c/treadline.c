#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    char* p;
    while ((p = readline(">>> "))) {
        printf("%p: %s\n", p, p);
        add_history(p);
        free(p);
    }
    printf("\n");

    return 0;
}
