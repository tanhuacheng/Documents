#!/usr/bin/python3
# -*- coding:utf-8

__author__ = 'tanhc'

def main ():
    profit = float(input('输入利润(万元): '))

    deduct = ((100, 0.01), (60, 0.015), (40, 0.03), (20, 0.05), (10, 0.075), (0, 0.1))

    result = 0
    for i in deduct:
        if profit > i[0]:
            result, profit = result + (profit - i[0]) * i[1], i[0]

    print('%.6f(万元)' % result)

if __name__ == '__main__':
    main()
