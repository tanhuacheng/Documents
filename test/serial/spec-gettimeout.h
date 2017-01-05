// spec-gettimeout.h
// 获取绝对超时时间(ms)

#ifndef SPEC_GETTIMEOUT_H_EQ4NPX50
#define SPEC_GETTIMEOUT_H_EQ4NPX50

#include <stdint.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

static inline void spec_gettimeout (struct timespec* ts, uint32_t timeout)
{
    uint64_t sec, nsc;

    sec = (timeout / 1000);
    nsc = (timeout % 1000) * 1000000;
    clock_gettime(CLOCK_REALTIME, ts);
    nsc += ts->tv_nsec;
    sec += (nsc / 1000000000);
    nsc %= 1000000000;
    ts->tv_nsec = nsc;
    ts->tv_sec += sec;
}

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: SPEC_GETTIMEOUT_H_EQ4NPX50 */
