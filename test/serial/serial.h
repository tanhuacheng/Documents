// serial.h
// 串口传输

#ifndef SERIAL_H_5HWNYSTJ
#define SERIAL_H_5HWNYSTJ

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

int serial_open (void);
int serial_read (void* buff, size_t size, int timeout);
int serial_write (const void* buff, size_t size);
int serial_tcdrain (void);

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: SERIAL_H_5HWNYSTJ */
