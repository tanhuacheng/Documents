#!/usr/bin/python3
# -*- coding:utf-8

'迭代器和生成器'

__author__ = 'tanhc'

import sys

def fib (n):
    a, b, c = 0, 1, 0
    while True:
        if (c > n):
            return
        yield a
        a, b = b, a + b
        c += 1

def test ():
    lst = [1, 2, 3, 4]
    itr = iter(lst)

    while True:
        try:
            print(next(itr))
        except StopIteration:
            break

    print('yield\n')
    f = fib(10)
    for i in f:
        print(i)

if __name__ == '__main__':
    test()
