#include <stdbool.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include "time-util.h"

static bool wait;
static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

static void* thread_timedwait (void* arg)
{
    (void)arg;

    pthread_mutex_lock(&mutex);
    wait = true;
    struct timespec ts;
    spec_gettimeout(&ts, 500);
    do {
        if (ETIMEDOUT == pthread_cond_timedwait(&cond, &mutex, &ts)) {
            if (!wait) {
                printf("signal\n");
            }
            wait = false;
            printf("ETIMEDOUT\n");
        }
    } while (wait);
    pthread_mutex_unlock(&mutex);

    pthread_exit(NULL);
    return NULL;
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    pthread_t tid;
    pthread_create(&tid, NULL, thread_timedwait, NULL);
    usleep(50 * 1000);

    pthread_mutex_lock(&mutex);
    usleep(800 * 1000);
    wait = false;
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);

    pthread_join(tid, NULL);

    return 0;
}
