#!/usr/bin/python3
# -*- coding:utf-8

'loop'

__author__ = 'tanhc'

def test ():
    sum = 0
    for i in range(1, 101):
        sum += i
    print(sum)

    count = 0
    while count < 5:
        print(sum)
        count += 1
    else:
        print(count)

    count = 0
    while count < 5: print(count); count += 1

    print('for')
    lst = [1, 3, 5, 7]
    for x in lst:
        print(x)
        if x <= 3:
            continue
        elif x > 3:
            break

    for i in range(len(lst)):
        print(lst[i])

if __name__ == '__main__':
    test()
