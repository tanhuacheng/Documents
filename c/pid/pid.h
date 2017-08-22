// pid.h

#ifndef PID_H_DV62VUBU
#define PID_H_DV62VUBU

#ifdef __cplusplus
extern "C" {
#endif

void* pid_create (void);
void pid_init (void* pid, float target, float kp, float ki, float kd);
float pid_control (void* pid, float in);
void pid_destroy (void* pid);

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: PID_H_DV62VUBU */
