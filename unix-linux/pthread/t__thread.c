#include <stdio.h>
#include <pthread.h>

static __thread int thread_specific = 1;

static void* thread_func (void* args)
{
    args = args;

    printf("%s, thread_specific = %d[%p]\n", __func__, thread_specific, &thread_specific);
    thread_specific = 2;

    return NULL;
}

int main (int argc, char* argv[])
{
    pthread_t tid;

    pthread_create(&tid, NULL, thread_func, NULL);
    pthread_join(tid, NULL);
    printf("%s, thread_specific = %d[%p]\n", __func__, thread_specific, &thread_specific);

    return 0;
}

/*
 * 测试结果:
 *
 * thread_func, thread_specific = 1[0x7f7ede32c6fc]
 * main, thread_specific = 1[0x7f7edeb1673c]
 *
 * */

