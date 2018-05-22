#!/usr/bin/python3

import random
import qsort

A = [random.randrange(0, 10) for i in range(10)]
qsort.qsort(A)
print(A)

B = [{'value': random.randrange(0,10)} for i in range(10)]
qsort.qsort(B, key=lambda x: -x['value'])
print(B)
