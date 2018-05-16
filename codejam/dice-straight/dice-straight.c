#include <assert.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

static int compare(const void *p1, const void *p2)
{
    int i1 = *(int*)p1;
    int i2 = *(int*)p2;

    return i1 < i2 ? -1 : i1 == i2 ? 0 : 1;
}

static int compare_nn(const void *p1, const void *p2, int n)
{
    int i1 = *(int*)p1;
    int i2 = *(int*)p2;

    if (i1 < i2) {
        return -1;
    }
    if (i1 > i2) {
        return 1;
    }

    if (0 == n) {
        return 0;
    }

    return compare_nn((int*)p1 + 1, (int*)p2 + 1, n - 1);
}

static int compare_n(const void *p1, const void *p2)
{
    return compare_nn(p1, p2, 5);
}

static void sort(int (*dices)[6], int N)
{
    for (int n = 0; n < N; n++) {
        qsort(dices[n], 6, sizeof(int), compare);
    }
    qsort(dices, N, sizeof(dices[0]), compare_n);

    int f = 0;
    for (int n = 1; n < N; n++) {
        if (dices[n-1][0] == dices[n][0] && dices[n-1][0] != 0x7fffffff) {
            dices[n-1][0] = 0x7fffffff;
            f = 1;
        }
    }

    if (f) {
        sort(dices, N);
    }
}

static int dice_straight(int (*dices)[6], int N)
{
    int max_straight = 0;

    for (int i = 0; i < N*6; i++) {
        sort(dices, N);

        int deep = 0;
        for (int n = 1; n < N; n++) {
            if (dices[n-1][0] + 1 == dices[n][0]) {
                deep++;
                if (deep > max_straight) {
                    max_straight = deep;
                }
            } else {
                deep = 0;
            }
        }

        dices[0][0] = 0x7fffffff;
    }

    return max_straight + 1;
}

static int test_cases(FILE *file)
{
    int N;
    fscanf(file, "%d", &N);

    int (*dices)[6] = calloc(N, sizeof(int) * 6);

    for (int n = 0; n < N; n++) {
        fscanf(file, "%d%d%d%d%d%d", &dices[n][0], &dices[n][1], &dices[n][2],
                                     &dices[n][3], &dices[n][4], &dices[n][5]);
    }

    int max_straight = dice_straight(dices, N);

    free(dices);

    return max_straight;
}

int main(int argc, char *argv[])
{
    assert(2 == argc);

    FILE *file = fopen(argv[1], "r");

    int T;
    fscanf(file, "%d", &T);
    for (int t = 1; t <= T; t++) {
        printf("Case #%d: %d\n", t, test_cases(file));
    }
    fclose(file);

    return 0;
}
