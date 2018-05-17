#include <assert.h>
#include <stdio.h>

static int flipper(char *s, int n, int k)
{
    int nflip = 0;

    for (int i = 0; i < n - k + 1; i++) {
        if (!s[i]) {
            nflip++;
            for (int j = i; j < i + k; j++) {
                s[j] = !s[j];
            }
        }
    }

    for (int i = n - k + 1; i < n; i++) {
        if (!s[i]) {
            return -1;
        }
    }

    return nflip;
}

int main(int argc, char *argv[])
{
    assert(2 == argc);

    FILE *file = fopen(argv[1], "r");
    int t;
    fscanf(file, "%d", &t);

    char s[1024];
    int k;
    for (int i = 1; i <= t; i++) {
        fscanf(file, "%s%d", s, &k);

        int n;
        for (n = 0; s[n]; n++) {
            s[n] = '+' == s[n];
        }

        printf("Case #%d: ", i);
        int r = flipper(s, n, k);
        if (r < 0) {
            printf("IMPOSSIBLE\n");
        } else {
            printf("%d\n", r);
        }
    }

    return 0;
}
