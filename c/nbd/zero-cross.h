// zero-cross.h
// 计算信号的短时过零数

#ifndef ZERO_CROSS_H_ZZQYCX41
#define ZERO_CROSS_H_ZZQYCX41

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ZERO_CROSS_DEAPI(T, S) \
size_t zero_cross_##S (const T* data, size_t size, T err)

ZERO_CROSS_DEAPI(int16_t, s);

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: ZERO_CROSS_H_ZZQYCX41 */
