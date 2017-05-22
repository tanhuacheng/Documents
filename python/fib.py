#!/usr/bin/python3
# -*- coding:utf-8

'Fibonacci series'

__author__ = 'tanhc'

def test ():
    a, b = 0, 1
    while b < 10:
        print(b, end=',')
        a, b = b, a + b

if __name__ == '__main__':
    test()
