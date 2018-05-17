#include <assert.h>
#include <stdio.h>

static char *last_tidy_number(char *buff)
{
    for (int i = 1; buff[i]; i++) {
        if (buff[i-1] > buff[i]) {
            int j;
            for (j = i-1; j > 0; j--) {
                if (buff[j]-1 >= buff[j-1]) {
                    break;
                }
            }
            buff[j] = buff[j] - 1;
            for (int k = j+1; buff[k]; k++) {
                buff[k] = '9';
            }
            break;
        }
    }

    while ('0' == *buff) buff++;

    return buff;
}

int main(int argc, char *argv[])
{
    assert(2 == argc);

    FILE *file = fopen(argv[1], "r");
    int t;
    fscanf(file, "%d", &t);

    char buff[32];
    for (int i = 1; i <= t; i++) {
        fscanf(file, "%s", buff);
        printf("Case #%d: %s\n", i, last_tidy_number(buff));
    }

    return 0;
}
