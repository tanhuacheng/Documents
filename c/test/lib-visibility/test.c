#include <stdio.h>

void foo (void);
void bar (void);

/*
 * 编译动态库时, 如果两个库包含相同的符号, 应用程序连接这两个库后, 只会连接其中的一个符号, 包括库本
 * 身使用这个符号时将产生非预期的结果. 例如 libfoo.so 和 libbar.so 中同时包含函数 dup, 当 libfoo.so
 * 中调用 dup 时, 实际可能调用的是 libbar.so 中的 dup, 反之亦然.
 *
 * 所以编译动态库时应当加入 -fvisibility=hidden 选项, 该选项导致库中的所有符号都是本地的, 除非手动指
 * 定某符号的可视性(例如: __attribute((visibility("default"))) 指明外部可以使用的符号).
 * */

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    foo();
    bar();

    return 0;
}

// end of file
