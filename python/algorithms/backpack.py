#!/usr/bin/python3

p = [0, 1, 6, 18, 22, 28]
v = [0, 1, 2, 5, 6, 7]

N = len(p) - 1
V = 11

m = [[0 for j in range(V+1)] for i in range(N+1)]

for i in range(1,N+1):
    for j in range(1, V+1):
        if j < v[i]:
            m[i][j] = m[i-1][j]
        else:
            q = m[i-1][j-v[i]] + p[i]
            m[i][j] = max(q, m[i-1][j])
print(m)
print(m[N][V])
