#!/usr/bin/python3

import heapq

#  lst = [1, 3, 2, 9, 7, 13, 5, 34, -1, 8]
#  print(heapq.nlargest(2, lst))
#  print(heapq.nsmallest(2, lst))
#
#
#  portfolio = [
#      {'name': 'IBM', 'shares': 100, 'price': 91.1},
#      {'name': 'AAPL', 'shares': 50, 'price': 543.22},
#      {'name': 'FB', 'shares': 200, 'price': 21.09},
#      {'name': 'HPQ', 'shares': 35, 'price': 31.75},
#      {'name': 'YHOO', 'shares': 45, 'price': 16.35},
#      {'name': 'ACME', 'shares': 75, 'price': 115.65}
#  ]
#
#  cheap = heapq.nsmallest(3, portfolio, key=lambda s: s['price'])
#  expensive = heapq.nlargest(3, portfolio, key=lambda s: s['price'])
#  print(cheap, expensive)

class PriorityQueue(object):

    def __init__(self):
        self._queue = []
        self._index = 0

    def push(self, item, prioriry):
        # tuple compare should return if previous compare is clear
        # it's impossible have the same value of self._index, so prevent the compare on item and
        # guarantee the first pushed item have high prioriry if it's prioriry value is the same
        heapq.heappush(self._queue, (-prioriry, self._index, item))
        self._index += 1

    def pop(self):
        try:
            prioriry, _, item = heapq.heappop(self._queue)
            return (item, -prioriry)
        except:
            return ()

q = PriorityQueue()
q.push('alice', 1)
q.push('bob', 3)
q.push('foo', 1)
q.push('bar', 3)

print(q.pop())
print(q.pop())
print(q.pop())
print(q.pop())
print(q.pop())
