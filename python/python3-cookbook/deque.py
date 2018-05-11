#!/usr/bin/python3

from collections import deque

q = deque(maxlen=4)
q.append(1)
q.append(2)
q.append(3)
q.append(4)
q.append(5)
print(q)

for x in q:
    print(x)
print(q)

print(q.pop())
print(q)

print(q.popleft())
print(q)
