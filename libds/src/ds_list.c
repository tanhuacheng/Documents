/*
*********************************************************************************************************
* 文件: ds_list.c
* 版本: V0.00
* 作者: 谭化成
* 时间: Fri Apr 29 18:29:48 2016
* 描述: 链表
*********************************************************************************************************
*/

/* 包含头文件 -----------------------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include "ds_list.h"

/* 私有宏定义 -----------------------------------------------------------------------------------------*/

/* 私有数据类型声明 -----------------------------------------------------------------------------------*/
struct ds_list_node {
    void* data;
    struct ds_list_node *next;
};

struct ds_list {
    struct ds_list_node* head;
    struct ds_list_node* tail;
    struct ds_list_node* indx;
    int data_length;
};

/* 私有全局变量定义 -----------------------------------------------------------------------------------*/

/* 函数定义 -------------------------------------------------------------------------------------------*/
struct ds_list* ds_list_create (const int data_length)
{
    struct ds_list* list = malloc(sizeof(struct ds_list));

    if (list == NULL) return NULL;

    list->head = NULL;
    list->tail = NULL;
    list->indx = NULL;
    list->data_length = data_length;

    return list;
}

/********************************************* end of file *********************************************/

