#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "goertzel.h"
#include "zero-cross.h"

#define SAMPLE_RATE 8000

static void generate (int16_t *in, int f, int n)
{
    memset(in, 0, 2 * n);
    for (int i = 0; i < n; i++) {
        in[i] += 4096 * sinf(0.5*M_PI*f*i / SAMPLE_RATE);
        in[i] += 8192 * sinf(2*M_PI*f*i / SAMPLE_RATE);
        // in[i] += 8192 * sinf(4*M_PI*f*i / SAMPLE_RATE);
    }
}

static float nbd (const int16_t *in, int n)
{
    float di = (float)SAMPLE_RATE / (2 * n);
    int zcn = zero_cross_s(in, n, 64);
    float fc = /*zero_cross_s(in, n, 64)*/ zcn * di;
    float pc = goertzel_filter(in, n, fc, SAMPLE_RATE);

    di *= 2.0f;
    for (; di > 0.1f; di *= 0.54) {
        float pl = goertzel_filter(in, n, fc - di, SAMPLE_RATE);
        float pr = goertzel_filter(in, n, fc + di, SAMPLE_RATE);

        float fm, pm;
        if (pl > pr) {
            fm = fc - di;
            pm = pl;
        } else {
            fm = fc + di;
            pm = pr;
        }

        if (pm > pc) {
            fc = fm;
            pc = pm;
        }
    }

    float s = 0;
    for (int i = 0; i < n; i++) {
        s += in[i] * in[i];
    }
    printf("%.2f: %.2f, %.2f\n", fc, 2*pc/n, s);

    return (2*pc/n) / s;
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    #define N 160

    int16_t in[N];
    for (int i = 3500; i <= 3500; i += 200) {
        generate(in, i, N);

    // for (int i = -10; i < 11; i++) {
    //     printf("%.2f\n", goertzel_filter(in, N, 975+i, 8000));
    // }

    float s = 0;
    for (int i = 0; i < N; i++) {
        s += in[i] * in[i];
    }
        // printf("%.2f %.2f\n", s, goertzel_filter(in, N, 1000, 8000));
        printf("%d: %.2f\n", i, nbd(in, N));
    }

    return 0;
}
