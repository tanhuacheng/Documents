#!/usr/bin/python3
# -*- coding:utf-8

'a test module'

__author__ = 'tanhc'

# import something

def printinfo1 (name, age = 24):
    print("名字:", name);
    print("年龄:", age);

def printinfo2 (arg1, *vartuple):
    print("输出: ");
    print(arg1)
    for var in vartuple:
        print(var)

a = 10

def sum_ (x, y, z = 5, *, a, b, **kw):
    print(kw)
    return x + y + z + a + b

def fact (n):
    if 1 == n:
        return 1
    return n * fact(n - 1)

def test ():
    print(a)

    print(sum_(1, 2, a = 1, b = 2, c = 3))

    printinfo1('tanhc', 35)
    printinfo2('sss', 1, 2, 3)

    sum = lambda arg1, arg2: arg1 + arg2
    print(sum(1, 20))

    print(fact(20))

if __name__ == '__main__':
    test()
