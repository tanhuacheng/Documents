// pid.c

/* 包含头文件 ------------------------------------------------------------------------------------*/
#include <stdlib.h>
#include "pid.h"

/* 私有数据类型声明 ------------------------------------------------------------------------------*/
struct pid {
    float target;
    float kp;
    float ki;
    float kd;
    float acc;
    float e;
};

/* 函数定义 --------------------------------------------------------------------------------------*/
void* pid_create (void)
{
    struct pid* pid = calloc(1, sizeof(struct pid));

    return pid;
}

void pid_init (void* pid, float target, float kp, float ki, float kd)
{
    struct pid* _pid = pid;
    _pid->target = target;
    _pid->kp = kp;
    _pid->ki = ki;
    _pid->kd = kd;
    _pid->acc = 0;
    _pid->e = 0;
}

float pid_control (void* pid, float in)
{
    struct pid* _pid = pid;
    float e = _pid->target - in;
    _pid->acc += e;

    float out = _pid->kp * e + _pid->ki * _pid->acc + _pid->kd * (e - _pid->e);
    _pid->e = e;

    return out;
}

void pid_destroy (void* pid)
{
    free(pid);
}

// end of file
