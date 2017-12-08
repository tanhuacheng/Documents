// goertzel.h

#ifndef GOERTZEL_H_BOOMRFPX
#define GOERTZEL_H_BOOMRFPX

#include <stdint.h>
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

static inline float goertzel_filter (const int16_t *in, int n, float f, float s)
{
    float s0 = 0.0f, s1 = 0.0f, s2 = 0.0f;
    float co = 2 * cosf(2*(float)M_PI*f / s);

    for (int i = 0; i < n; i++) {
        s0 = in[i] + co * s1 - s2;
        s2 = s1;
        s1 = s0;
    }

    return s2*s2 + s1*s1 - co*s1*s2;
}

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: GOERTZEL_H_BOOMRFPX */
