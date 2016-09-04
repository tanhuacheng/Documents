#include <stdio.h>

int main (int argc, char* argv[])
{
    const int foo = 0;

    printf("const foo = %d\n", foo);
    *(int*)&foo = 1; // static 或 全局变量 将出错
    printf("const foo = %d\n", foo);

    return 0;
}
