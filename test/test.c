#include <stdio.h>
#include <string.h>

int main (int argc, char* argv[])
{
    printf("memcmp tanhuacheng to tanhuacheng = %d\n", memcmp("tanhuacheng", "tanhuacheng", strlen("tanhuacheng")));
    printf("memcmp sanhuacheng to tanhuacheng = %d\n", memcmp("sanhuacheng", "tanhuacheng", strlen("tanhuacheng")));
    printf("memcmp tanhuacheng to sanhuacheng = %d\n", memcmp("tanhuacheng", "sanhuacheng", strlen("tanhuacheng")));

    return 0;
}

