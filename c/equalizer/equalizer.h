// equalizer.h

#ifndef EQUALIZER_H_60HDITNT
#define EQUALIZER_H_60HDITNT

#include <stdbool.h>
#include "equalizer_presets.h"

#ifdef __cplusplus
extern "C" {
#endif

void* EqzInit (int rate, bool twopass, const eqz_preset_t* preset);

void EqzFilter(void* inst, float *out, float *in, int samples, int channels);

void EqzClean (void* inst);

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: EQUALIZER_H_60HDITNT */
