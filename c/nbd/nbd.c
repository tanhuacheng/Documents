#include <stdint.h>
#include <stdio.h>
#include <math.h>

static void gen_sinf (int16_t *in, int f, int n)
{
    for (int i = 0; i < n; i++) {
        in[i] = 1000 * sinf(2*M_PI*f*i/8000) + 0.1;
    }
}

static void print (const int16_t *in, int n)
{
    for (int i = 0; i < n; i++) {
        if (!(i%16)) printf("\n");
        printf("% 4d ", in[i]);
    }
    printf("\n");
}

static float nbd (const int16_t *in, int n)
{
    int i = 1;
    for (; i < n; i++) {
        if ((in[i] ^ in[i - 1]) & 0x8000)  {
            break;
        }
    }

    float p[n];
    int z[n];
    int c = 0;

    int b = i;
    p[c] = 0;

    float sp = 0;
    int sz = 0;

    for (; i < n - 1; i++) {
        p[c] += in[i] * in[i];
        if ((in[i] ^ in[i + 1]) & 0x8000) {
            z[c] = i + 1 - b;
            b = i + 1;
            sp += p[c];
            sz += z[c];
            c++;
            p[c] = 0;
        }
    }

    float ap = sp / c;
    float az = sz / c;

    float ep = 0;
    float ez = 0;

    for (int i = 0; i < c; i++) {
        printf("%.2f, %d\n", p[i], z[i]);
        ep += powf(p[i] - ap, 2);
        ez += powf(z[i] - az, 2);
    }

    ep /= c;
    ez /= c;

    printf("%.6f %.6f\n", sqrtf(sqrtf(ep / (ap * ap))), sqrtf(ez / (az * az)));

    return 0;
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    #define N 80

    int16_t in[N];
    gen_sinf(in, 2000, N);

    print(in, N);
    nbd(in, N);

    return 0;
}
