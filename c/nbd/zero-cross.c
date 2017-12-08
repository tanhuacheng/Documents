// zero-cross.c
// 计算信号的短时过零数

#include <stdbool.h>
#include "zero-cross.h"

#define ZERO_CROSS_DEFUN(T, S) \
ZERO_CROSS_DEAPI(T, S) { \
    if ((NULL == data) || (0 == size) || (err < 0)) { \
        return 0; \
    } \
\
    size_t nzc = 0; \
    bool sig = data[0] < 0; \
\
    for (size_t i = 1; i < size; i++) { \
        if (sig) { \
            if (data[i] > err) { \
                nzc++; \
                sig = false; \
            } \
        } else { \
            if (data[i] < -err) { \
                nzc++; \
                sig = true; \
            } \
        } \
    } \
\
    return nzc; \
}

ZERO_CROSS_DEFUN(int16_t, s)

/****************************************** end of file *******************************************/
