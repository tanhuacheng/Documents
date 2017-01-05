// serial.h
// 串口传输

/* 包含头文件 ------------------------------------------------------------------------------------*/
#include <stdlib.h>
#include <time.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <pthread.h>

#include "spec-gettimeout.h"

/* 私有全局变量定义 ------------------------------------------------------------------------------*/
static int fd_serial  = -1;
static pthread_mutex_t mutex_recv = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mutex_send = PTHREAD_MUTEX_INITIALIZER;

/* 函数定义 --------------------------------------------------------------------------------------*/
int serial_open (void)
{
#ifdef ANDROID_NDK
#   define EXIT_TIMEDLOCK(lock, timeout) \
    do { \
        if (pthread_mutex_lock_timeout_np(lock, timeout)) { \
            exit(1); \
        } \
    } while (0)
#else
#   define EXIT_TIMEDLOCK(lock, timeout) \
    do { \
        struct timespec ts; \
        spec_gettimeout(&ts, timeout); \
        if (pthread_mutex_timedlock(lock, &ts)) { \
            exit(1); \
        } \
    } while (0)
#endif

    EXIT_TIMEDLOCK(&mutex_recv, 5000);
    EXIT_TIMEDLOCK(&mutex_send, 5000);

#   define RETURN_UNLOCK(r) \
    do { \
        pthread_mutex_unlock(&mutex_send); \
        pthread_mutex_unlock(&mutex_recv); \
        return r; \
    } while (0)

    if (fd_serial >= 0) {
        tcflush(fd_serial, TCIOFLUSH);
        RETURN_UNLOCK(0);
    }

    int fd = open("/dev/ttyS0", O_RDWR | O_NOCTTY);
    if (fd < 0) {
        RETURN_UNLOCK(-1);
    }

    struct termios opt;
    tcgetattr(fd, &opt);

    cfsetispeed(&opt, B115200);
    cfsetospeed(&opt, B115200);

    tcflush(fd, TCIOFLUSH);
    if (tcsetattr(fd, TCSANOW, &opt) != 0) {
        close(fd);
        RETURN_UNLOCK(-1);
    }
    tcflush(fd, TCIOFLUSH);

    opt.c_cflag &= ~(PARENB | CSTOPB | CSIZE | CRTSCTS);
    opt.c_cflag |= (CLOCAL | CREAD | CS8);
    opt.c_lflag &= ~(ICANON | ISIG | IEXTEN | ECHO | ECHONL);
    opt.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | INPCK);
    opt.c_iflag &= ~(IXON | IXOFF | IXANY);
    opt.c_oflag &= ~OPOST;
    opt.c_cc[VTIME] = 0;
    opt.c_cc[VMIN] = 0;

    tcflush(fd, TCIOFLUSH);
    if (tcsetattr(fd, TCSANOW, &opt) != 0) {
        close(fd);
        RETURN_UNLOCK(-1);
    }
    tcflush(fd, TCIOFLUSH);

    fd_serial = fd;
    RETURN_UNLOCK(0);
}

#define RETURN_NOT_OPEN_ELSE_LOCK(lock) \
do { \
    pthread_mutex_lock(lock); \
    if (fd_serial < 0) { \
        pthread_mutex_unlock(lock); \
        return -1; \
    } \
} while (0)

#define RETURN_PARAM_ERROR_UNLOCK(lock) \
do { \
    if ((NULL == buff) || (0 == size)) { \
        pthread_mutex_unlock(lock); \
        return 0; \
    } \
} while (0)

int serial_read (void* buff, size_t size, int timeout)
{
    RETURN_NOT_OPEN_ELSE_LOCK(&mutex_recv);
    RETURN_PARAM_ERROR_UNLOCK(&mutex_recv);

    int result = 0;

    if (timeout < 0) {
        result = read(fd_serial, buff, size);
    } else {
        struct timeval tv = {
            .tv_sec = timeout / 1000,
            .tv_usec = (timeout % 1000) * 1000,
        };

        fd_set fs;
        FD_ZERO(&fs);
        FD_SET(fd_serial, &fs);

        int res = select(fd_serial + 1, &fs, NULL, NULL, timeout ? &tv : NULL);
        if (res > 0) {
            result = read(fd_serial, buff, size);
        } else {
            result = res;
        }
    }
    pthread_mutex_unlock(&mutex_recv);

    return result;
}

int serial_write (const void* buff, size_t size)
{
    RETURN_NOT_OPEN_ELSE_LOCK(&mutex_send);
    RETURN_PARAM_ERROR_UNLOCK(&mutex_send);

    fd_set fs;
    FD_ZERO(&fs);
    FD_SET(fd_serial, &fs);

    int result = 0;

    int res = select(fd_serial + 1, NULL, &fs, NULL, NULL);
    if (res > 0) {
        result = write(fd_serial, buff, size);
    } else {
        result = res;
    }
    pthread_mutex_unlock(&mutex_send);

    return result;
}

int serial_tcdrain (void)
{
    RETURN_NOT_OPEN_ELSE_LOCK(&mutex_send);
    int result = tcdrain(fd_serial);
    pthread_mutex_unlock(&mutex_send);

    return result;
}

/****************************************** end of file *******************************************/
