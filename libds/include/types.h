/*
*********************************************************************************************************
* 文件: types.h
* 版本: V1.00
* 作者: 谭化成
* 描述: 平台无关的数据类型定义
*
* 创建作者: 谭化成
* 创建时间: Sun Jun 21 17:05:51 2015
* 版本历史
* +------------------------------------------------------------------------------------------------+
* |    时间    |  版本  |                            描 述                            |    作者    |
* +------------------------------------------------------------------------------------------------+
* | 2015/06/21 |  1.00  | 初版完成                                                    |   谭化成   |
* +------------------------------------------------------------------------------------------------+
*
*********************************************************************************************************
*/

#ifndef  TYPES_H_
#define  TYPES_H_

#ifdef   __cplusplus
  extern "C" {
#endif

typedef unsigned char       uint8_t;                        /* 无符号  8位 整  型 变量 */
typedef signed   char       int8_t;                         /* 有符号  8位 整  型 变量 */
typedef unsigned short      uint16_t;                       /* 无符号 16位 整  型 变量 */
typedef signed   short      int16_t;                        /* 有符号 16位 整  型 变量 */
typedef unsigned int        uint32_t;                       /* 无符号 32位 整  型 变量 */
typedef signed   int        int32_t;                        /* 有符号 32位 整  型 变量 */
typedef float               fp32_t;                         /* 单精度 32位 浮点型 变量 */
typedef double              fp64_t;                         /* 双精度 64位 浮点型 变量 */

#ifndef BOOL
#define BOOL                uint8_t                         /* 布尔型变量              */
#endif
#ifndef TRUE
#define TRUE                (1u)                            /* 逻辑真                  */
#endif
#ifndef FALSE
#define FALSE               (0u)                            /* 逻辑假                  */
#endif

#ifdef  __cplusplus
  }
#endif

#endif  /* TYPES_H_ */
/********************************************* end of file *********************************************/
