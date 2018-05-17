#!/usr/bin/python3

import sys

assert(2 == len(sys.argv))

def alphabet_cake(g):
    for i in range(len(g)):
        for j in range(len(g[0])):
            if g[i][j] != '?' and g[i][j].isupper():

                p = j - 1
                while p >= 0 and g[i][p] == '?':
                    g[i][p] = g[i][j].lower()
                    p -= 1

                n = j + 1
                while n < len(g[0]) and g[i][n] == '?':
                    g[i][n] = g[i][j].lower()
                    n += 1

                t = i - 1
                while t >= 0 and all(map(lambda x: x == '?', g[t][p+1:n])):
                    for k in range(p+1, n):
                        g[t][k] = g[i][j].lower()
                    t -= 1

                b = i + 1
                while b < len(g) and all(map(lambda x: x == '?', g[b][p+1:n])):
                    for k in range(p+1, n):
                        g[b][k] = g[i][j].lower()
                    b += 1

    for i in range(len(g)):
        print(''.join(map(lambda x: x.upper(), g[i])))

with open(sys.argv[1], 'r') as f:
    t = int(f.readline())
    for i in range(1, t+1):
        r, *_ = map(int, f.readline().split(' '))
        g = []
        for j in range(r):
            g.append(list(map(lambda x: x, f.readline().strip())))
        print('Case #{}:'.format(i))
        alphabet_cake(g)
