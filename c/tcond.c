#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

static bool wait = false;
static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

static void* thread_wait (void* arg)
{
    (void)arg;

    while (1) {
        pthread_mutex_lock(&mutex);
        wait = true;
        do {
            pthread_cond_wait(&cond, &mutex);
        } while (wait);
        pthread_mutex_unlock(&mutex);
    }

    return NULL;
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    pthread_create(&(pthread_t){0}, NULL, thread_wait, NULL);
    sleep(1);

    unsigned long count = 0;
    while (1) {
        pthread_mutex_lock(&mutex);
        if (!wait) {
            printf("unexpected %ld!\n", count);
            return 0;
        }
        count++;
        wait = false;
        pthread_cond_signal(&cond);
        pthread_mutex_unlock(&mutex);
    }

    return 0;
}
