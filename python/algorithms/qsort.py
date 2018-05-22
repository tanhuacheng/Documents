import random

def _partition(A, p, r, key):

    def _swap(i, j):
        t = A[i]
        A[i] = A[j]
        A[j] = t

    def _value(x):
        return key(x) if callable(key) else x

    i = random.randrange(p, r)
    _swap(i, r - 1)

    i = p - 1
    for j in range(p, r - 1):
        if _value(A[j]) <= _value(A[r - 1]):
            i += 1
            _swap(i, j)

    i += 1
    _swap(i, r - 1)

    return i

def _qsort(A, p, r, key):
    if p == r:
        return

    q = _partition(A, p, r, key)
    _qsort(A, p, q, key)
    _qsort(A, q + 1, r, key)

def qsort(A, key=None):
    _qsort(A, 0, len(A), key)
