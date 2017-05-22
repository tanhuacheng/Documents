#include <stdio.h>

#define lambda(l_ret_type, l_arguments, l_body) ({ \
    l_ret_type l_anonymous_functions_name l_arguments \
        l_body \
    &l_anonymous_functions_name; \
})

void func (int n, void (*f) (void))
{
    for (int i = 0; i < n; i++) f();
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    func(4, lambda(void, (void), { static int i = 0; printf("hi %d\n", i++); }));

    return 0;
}
