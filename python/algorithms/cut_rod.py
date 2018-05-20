#!/usr/bin/python3

import sys

p = (0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30)
n = int(sys.argv[1])

r = [0 for i in range(n+1)]
s = [0 for i in range(n+1)]

for i in range(1, n+1):
    for j in range(1, i+1):
        if r[i] < p[j] + r[i-j]:
            r[i] = p[j] + r[i-j]
            s[i] = j

print(r[n])
while n > 0:
    print(s[n], end=' ')
    n -= s[n]
print()
