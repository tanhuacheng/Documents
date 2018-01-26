#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

static void *test_arg = NULL;

static void* thread_test (void* arg)
{
    void (*cb) (void*) = arg;

    while (1) {
        cb(test_arg);
        sleep(1);
    }

    return NULL;
}

int foo (void (*cb) (void*), void *arg)
{
    test_arg = arg;

    printf("c: %s %ld\n", __func__, (long)arg);
    pthread_create(&(pthread_t){0}, NULL, thread_test, cb);

    return 0x55;
}
