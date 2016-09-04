#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include "high_pass_filter.h"

static void print_data (const int16_t* data, int length)
{
    int data_power = 0;

    for (int i = 0; i < length; i++) {
        if (0 == (i % 16)) {
            printf("\n");
        }
        printf("%6d ", data[i]);
        data_power += abs(data[i]);
    }
    printf("\n%d\n", data_power / length);
}

static void test (int frq, int smpfrq, int length)
{
    struct high_pass_filter* filter = high_pass_filter_create(smpfrq);
    assert(NULL != filter);

    int16_t data[length];

    printf("\n%dHz @ %d\n", frq, smpfrq);
    for (int i = 0; i < length; i++) {
        data[i] = 8192 * sin((i * 2.0 * M_PI * frq) / smpfrq);
    }
    print_data(data, length);

    assert(0 == high_pass_filter_process(filter, data, length));
    print_data(data, length);

    high_pass_filter_destroy(filter);
}

int main (int argc, char* argv[])
{
    test(1000, 8000, 160);
    test(100, 8000, 160);
    test(10, 8000, 800);

    return 0;
}
