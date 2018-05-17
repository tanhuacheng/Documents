#!/usr/bin/python3

import sys

assert(len(sys.argv) == 2)

def bathroom_stalls(n, k):
    s = {n}
    c = {n:1}
    p = 0
    while p < k:
        x = max(s)
        max_, min_ = x//2, (x-1)//2
        p += c[x]

        if max_ in s:
            c[max_] += c[x]
        else:
            s.add(max_)
            c[max_] = c[x]

        if min_ in s:
            c[min_] += c[x]
        else:
            s.add(min_)
            c[min_] = c[x]

        c.pop(x)
        s.remove(x)

    return max_, min_

with open(sys.argv[1], "r") as f:
    t = int(f.readline())
    for i in range(1, t+1):
        n, k = map(int, f.readline().split(' '))
        max_, min_ = bathroom_stalls(n, k)
        print('Case #{}: {} {}'.format(i, max_, min_))
