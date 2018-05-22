#!/usr/bin/python3

from functools import reduce

p = (30, 35, 15, 5, 10, 20, 25)
#  p = (10, 100, 5, 50)

n = len(p) - 1
m = [[0 for j in range(n - i, 0, -1)] for i in range(n)]
s = [[0 for j in range(n - i, 1, -1)] for i in range(n - 1)]

for l in range(2, n + 1):
    for i in range(n - l + 1):
        j = i + l - 1
        m[i][j - i] = reduce(lambda x, y: x * y, p)
        for k in range(i, j):
            q = m[i][k - i] + m[k + 1][j - k - 1] + p[i] * p[k + 1] * p[j + 1]
            if q < m[i][j - i]:
                m[i][j - i] = q
                s[i][j - i - 1] = k

print(p)
print(m)
print(s)

def print_parens(s, i, j):
    if i == j:
        print('A{}'.format(i+1), end='')
    else:
        print('(', end='')
        print_parens(s, i, s[i][j-i-1])
        print_parens(s, s[i][j-i-1]+1, j)
        print(')', end='')

print_parens(s, 0, n-1)
print()
