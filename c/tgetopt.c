#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// ./a.out -h -o xxx -v sss abc 123 -v

int main(int argc, char *argv[])
{
    const char* optstring = "vho:";

    while (1) {
        int opt = getopt(argc, argv, optstring);
        if (-1 == opt) {
            break;
        }

        switch (opt) {
            case 'v': printf("-v\n"); break;
            case 'h': printf("-h\n"); break;
            case 'o': printf("-o %s\n", optarg); break;
            case '?': printf("unknow\n"); break;
            default: printf("default\n"); break;
        }
    }

    printf("\nrest:\n");
    while (optind < argc) {
        printf("%s\n", argv[optind++]);
    }

    return 0;
}
