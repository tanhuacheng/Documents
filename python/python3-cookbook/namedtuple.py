#!/usr/bin/python3

from collections import namedtuple

Stock = namedtuple('Stock', ['name', 'shares', 'price'])

def compute_cost(records):
    total = 0.0
    for rec in records:
        s=Stock(*rec)
        total += s.shares * s.price
    return total

records = [('a', 1.0, 3.0), ('b', 2.4, 2.1)]
print(compute_cost(records))
