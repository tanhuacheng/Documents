// getmac.c
// 获取物理地址

#include <stdio.h>

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    FILE* file = popen("./mac", "r");
    char buff[128] = {0};

    while (fgets(buff, sizeof(buff), file)) {
        printf("%s", buff);
    }

    pclose(file);

    return 0;
}
