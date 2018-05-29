#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <wait.h>
#include <unistd.h>

#define MAXLINE 256

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    char buff[MAXLINE];

    printf("%% ");
    while(fgets(buff, sizeof(buff), stdin) != NULL) {
        if (buff[strlen(buff) - 1] == '\n') {
            buff[strlen(buff) - 1] = '\0';
        }

        pid_t pid;
        if ((pid = fork()) < 0) {
            perror("fork");
            exit(1);
        } else if (pid == 0) {
            execlp(buff, buff, (char*)NULL);
            perror("execlp");
            exit(127);
        }

        int status;
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            perror("waitpid");
            exit(1);
        }

        printf("%% ");
    }

    return 0;
}
