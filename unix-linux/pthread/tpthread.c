#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

int main (int argc, char* argv[])
{
    printf("begain join self\n");
    int res = pthread_join(pthread_self(), NULL);
    printf("end join self, res = %d\n", res);

    return 0;
}

