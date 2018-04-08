// h264-packetizer.c

/* 包含头文件 ------------------------------------------------------------------------------------*/
#include <stdint.h>
#include <stdlib.h>
#include "h264-packetizer.h"

/* 私有数据类型声明 ------------------------------------------------------------------------------*/
struct nal_header {
    uint8_t type:5;
    uint8_t nri:2;
    uint8_t f:1;
};

/* 函数定义 --------------------------------------------------------------------------------------*/
struct h264_packetizer *h264_packetizer_create(void)
{
    return NULL;
}

// test
int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    return 0;
}

// end of file
