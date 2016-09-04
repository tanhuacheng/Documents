// high_pass_filter.c

#include <stdlib.h>
#include <string.h>

#include "high_pass_filter.h"

#define WEBRTC_SPL_SAT(a, b, c) (b > a ? a : b < c ? c : b)

static const int16_t coefficients_8khz[5] = {3798, -7596, 3798, 7807, -3733};
static const int16_t coefficients[5] = {4012, -8024, 4012, 8002, -3913};

struct high_pass_filter {
  int16_t y[4];
  int16_t x[2];
  const int16_t* ba;
};

struct high_pass_filter* high_pass_filter_create (int sample_rate_hz)
{
    if (sample_rate_hz < 0) {
        return NULL;
    }

    struct high_pass_filter* filter = malloc(sizeof(struct high_pass_filter));

    if (NULL == filter) {
        return NULL;
    }

    if (8000 == sample_rate_hz) {
        filter->ba = coefficients_8khz;
    } else {
        filter->ba = coefficients;
    }

    memset(filter->x, 0, sizeof(filter->x));
    memset(filter->y, 0, sizeof(filter->y));

    return filter;
}

int high_pass_filter_process (struct high_pass_filter* filter, int16_t* data, int length)
{
    if ((NULL == filter) || (NULL == data) || (length < 0)) {
        return -1;
    }

    int32_t tmp_int32 = 0;
    int16_t* y = filter->y;
    int16_t* x = filter->x;
    const int16_t* ba = filter->ba;

    for (int i = 0; i < length; i++) {
        //  y[i] = b[0] * x[i] + b[1] * x[i-1] + b[2] * x[i-2] + -a[1] * y[i-1] + -a[2] * y[i-2];
        tmp_int32 = y[1] * ba[3];       // -a[1] * y[i-1] (low part)
        tmp_int32 += y[3] * ba[4];      // -a[2] * y[i-2] (low part)
        tmp_int32 = (tmp_int32 >> 15);

        tmp_int32 += y[0] * ba[3];      // -a[1] * y[i-1] (high part)
        tmp_int32 += y[2] * ba[4];      // -a[2] * y[i-2] (high part)
        tmp_int32 = (tmp_int32 << 1);
 
        tmp_int32 += data[i] * ba[0];   // b[0] * x[0]
        tmp_int32 += x[0] * ba[1];      // b[1] * x[i-1]
        tmp_int32 += x[1] * ba[2];      // b[2] * x[i-2]
 
        // Update state (input part)
        x[1] = x[0];
        x[0] = data[i];
 
        // Update state (filtered part)
        y[2] = y[0];
        y[3] = y[1];
        y[0] = (int16_t)(tmp_int32 >> 13);
        y[1] = (int16_t)((tmp_int32 - ((int32_t)y[0] << 13)) << 2);
 
        // Rounding in Q12, i.e. add 2^11
        tmp_int32 += 2048;
 

        // Saturate (to 2^27) so that the HP filtered signal does not overflow
        tmp_int32 = WEBRTC_SPL_SAT((int32_t)134217727, tmp_int32, (int32_t)-134217728);
 
        // Convert back to Q0 and use rounding
        data[i] = (int16_t)(tmp_int32 >> 12);
    }
 
    return 0;
}

void high_pass_filter_destroy (struct high_pass_filter* filter)
{
    if (NULL != filter) {
        free(filter);
    }
}

// end of file
