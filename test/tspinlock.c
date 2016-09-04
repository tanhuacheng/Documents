#include <stdio.h>
#include <pthread.h>

static int sum = 0;
static pthread_spinlock_t lock;

static void* thread_add (void* args)
{
    args = args;

    for (int i = 0; i < 100000; i++) {
        pthread_spin_lock(&lock);
        sum = sum + 1;
        pthread_spin_unlock(&lock);
    }

    printf("%s, exit\n", __func__);

    return NULL;
}

int main (int argc, char* argv[])
{
    pthread_spin_init(&lock, 1);

    pthread_t tid;
    pthread_create(&tid, NULL, thread_add, NULL);

    long int result = 0;
    for (int i = 0; i < 100000; i++) {
        pthread_spin_lock(&lock);
        result += sum;
        pthread_spin_unlock(&lock);
    }

    pthread_join(tid, NULL);
    printf("result = %ld\n", result);

    return 0;
}
