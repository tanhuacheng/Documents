#include "ds_list.h"
#include <stdio.h>

int main (int argc, char* argv[])
{
    struct ds_list* list = ds_list_create(4);
    printf("%p\n", list);
    free(list);
    return 0;
}
