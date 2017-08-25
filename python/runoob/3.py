#!/usr/bin/python3
# -*- coding:utf-8

__author__ = 'tanhc'

def main ():
    ''' x + 100 = n^2; n^2 + 168 = m^2; resolve x
        let m = n + k
        then 2nk = 168 - k^2
               n = 84/k - k/2
        1 <= k^2 <= 168
        1 <= k <= 12
    '''

    for k in range(1, 13):
        n = 84/k - k/2
        if int(n) == n:
            print(n*n - 100)

if __name__ == '__main__':
    main()
