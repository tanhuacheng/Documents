#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "pid.h"

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    srand(time(NULL));

    void* pid = pid_create();
    pid_init(pid, 100, 0.02, 0.05, 0.001);

    float in = 0;
    for (int i = 0; i < 200; i++) {
        float out = pid_control(pid, in);
        printf("%.2f, %.2f\n", in, out);
        in = in * 0.8 + out * out * 0.2 + 0.001 * ((rand() % 100) - 50);
    }
    pid_destroy(pid);

    return 0;
}
