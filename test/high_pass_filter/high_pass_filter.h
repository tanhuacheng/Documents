// high_pass_filter.h

#pragma once

#include <stdint.h>

#ifdef cplusplus
extern "C" {
#endif

struct high_pass_filter;

struct high_pass_filter* high_pass_filter_create (int sample_rate_hz);
int high_pass_filter_process (struct high_pass_filter* filter, int16_t* data, int length);
void high_pass_filter_destroy (struct high_pass_filter* filter);

#ifdef cplusplus
}
#endif

// end of file
