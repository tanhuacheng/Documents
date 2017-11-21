#include <stdio.h>
#include <math.h>
#include "equalizer.h"

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    int rate = 8000;

    #define N 1024

    eqz_preset_t preset = {
        "", 10, 12.f,
        // { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
        // 31.25, 62.5, 125, 250, 500, 1000, 2000, 4000, 8000, 16000
        {-3.0f, -2.0f, -1.0f, 0.0f, 0.0f, 0.0f, 1.5f, 3.0f, 3.0f, 3.0f },
    };

    // void* eqz = EqzInit(rate, true, &eqz_preset_10b[0]);
    void* eqz = EqzInit(rate, false, true, &preset);

    float in[N];
    float out[N];

    #define M 40
    float f = 50;
    float g = powf(2, log2f(80) / M);

    for (int i = 0; i < M + 1; i++) {
        for (int j = 0; j < N; j++) {
            in[j] = 1417 * sin(2*M_PI*f*j/rate);
        }
        EqzFilter(eqz, out, in, N, 1);

        float power_i = 0;
        float power_o = 0;
        for (int i = N*2/3; i < N; i++) {
            power_i += in[i] * in[i];
            power_o += out[i] * out[i];
        }

        printf("% 8.2f % 8.2f % 8.2f\n", f, sqrtf(power_i * 3 / N), sqrtf(power_o * 3 / N));
        f *= g;
    }
    printf("\n");

    return 0;
}
