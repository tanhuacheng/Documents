#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

static void* thread_test (void* arg)
{
    void (*cb) (void) = arg;

    while (1) {
        cb();
        sleep(1);
    }

    return NULL;
}

int foo (void (*cb) (void))
{
    printf("c: %s\n", __func__);
    pthread_create(&(pthread_t){0}, NULL, thread_test, cb);

    return 0x55;
}
