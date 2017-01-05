/*
 * =====================================================================================
 *
 *       Filename:  test.c
 *
 *    Description:  test
 *
 *        Version:  1.0
 *        Created:  2016年10月26日 17:34
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  tanhuacheng (tanhc), tanhc.gz@gmail.com
 *   Organization:  telephone - Android JNI
 *
 * =====================================================================================
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "serial.h"

#define PRINT_EXP(exp) \
do { \
    printf(#exp ": %d\n", exp); \
} while (0)

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    if (serial_open() < 0) {
        perror("serial_open");
        exit(0);
    }

    static char buff[160] = {'\0'};

//    assert(serial_read(NULL, 0, 0) == 0);
//    assert(serial_read(buff, 160, -1) >= 0);
//    assert(serial_read(buff, 160, 1000) >= 0);
//    assert(serial_write(buff, 160) >= 0);
//    assert(serial_tcdrain() == 0);

    PRINT_EXP(serial_read(NULL, 0, 0));
    PRINT_EXP(serial_read(buff, 160, -1));
    PRINT_EXP(serial_read(buff, 160, 1000));
    PRINT_EXP(serial_write(buff, 160));
    PRINT_EXP(serial_tcdrain());
    PRINT_EXP(serial_read(buff, 160, 0)); // 一直等待, 直到接收到数据

    return 0;
}
