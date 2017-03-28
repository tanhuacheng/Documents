// time-util.h
// 时间相关功能

#ifndef TIME_UTIL_H_S1ROIPQX
#define TIME_UTIL_H_S1ROIPQX

#include <time.h>
#include <sys/time.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned long int msecond_t;

static inline msecond_t msec_gettime (void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (ts.tv_sec * 1000ul) + (ts.tv_nsec / 1000000ul);
}

static inline void spec_gettimeout (struct timespec* tsp, msecond_t timeout)
{
    msecond_t sec = (timeout / 1000);
    msecond_t nsc = (timeout % 1000) * 1000000;
    clock_gettime(CLOCK_REALTIME, tsp);
    nsc += tsp->tv_nsec;
    sec += (nsc / 1000000000);
    nsc %= 1000000000;
    tsp->tv_nsec = nsc;
    tsp->tv_sec += sec;
}

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: TIME_UTIL_H_S1ROIPQX */
