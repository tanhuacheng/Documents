#!/usr/bin/python3
# -*- coding:utf-8

'a test module'

__author__ = 'tanhc'

import random

def test ():

    x = random.randrange(0, 100)
    y = random.randrange(0, 200)

    if x > y:
        print('x', x)
    elif x == y:
        print('x + y', x + y)
    else:
        print('y', y)

if __name__ == '__main__':
    test()
