#!/usr/bin/python3
# -*- coding:utf-8

__author__ = 'tanhc'

import itertools

def test ():
    lst = []
    rng = range(1, 5)
    for i in rng:
        for j in rng:
            for k in rng:
                if i != j and i != k and j != k:
                    lst.append((i, j, k))
    print(lst, len(lst))

    lst = [i * 100 + j * 10 + k for i in range(1, 5) for j in range(1, 5) for k in range(1, 5)
            if i != j and i != k and j != k]
    print(lst, len(lst))

    lst = []
    for i in itertools.permutations([1, 2, 3, 4], 3):
        lst.append(i)
    print(lst, len(lst))

    lst = [i for i in itertools.permutations(range(1, 5), 3)]
    print(lst, len(lst))

    lst = list(itertools.permutations(range(1, 5), 3))
    print(lst, len(lst))

if __name__ == '__main__':
    test()
