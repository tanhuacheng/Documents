    c                                           c++

    sizeof('a') => sizeof(int)                  sizeof('a') => sizeof(char)
    result of assignment is an rvalue           result of assignment is an lvalue
    ++i is an rvalue                            ++i is an lvalue
    i++ is an rvalue                            i++ is an rvalue
    (a, b, c): rvalue equal to c                (a, b, c): lvalue if c is an lvalue
