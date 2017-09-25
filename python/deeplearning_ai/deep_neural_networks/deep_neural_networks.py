#!/usr/bin/python3
# -*- coding:utf-8

'deep neural networks'

__author__ = 'tanhuacheng'

import numpy as np

def train(x, y, layers, activations, differentials, loss, dloss, alpha, lambd, iterations):
    assert(np.shape(y)[1] == np.shape(x)[1])
    assert(np.shape(y)[0] == layers[-1])
    assert(len(layers) == len(activations) == len(differentials))

    m = np.shape(x)[1]

    w = [None]
    b = [None]
    z = [None]
    a = [x]

    l, g, d = list(layers), list(activations), list(differentials)
    l.insert(0, np.shape(x)[0])
    g.insert(0, None)
    d.insert(0, None)
    L = len(l)

    da, dz, dw, db = ([None] for i in range(4))
    for i in range(1, L):
        w.append(np.random.randn(l[i], l[i - 1]) * np.sqrt(1 / l[i - 1]))
        b.append(np.random.randn(l[i], 1))
        z.append(None)
        a.append(None)

        da.append(0)
        dz.append(0)
        dw.append(0)
        db.append(0)

    cost = []
    for it in range(iterations):
        for i in range(1, L):
            z[i] = np.dot(w[i], a[i - 1]) + b[i]
            a[i] = g[i](z[i])

        cost.append(np.sum(loss(a[-1], y)) / m)
        da[-1] = dloss(a[-1], y)

        for i in range(1, L):
            dz[-i] = da[-i] * d[-i](z[-i], a[-i])
            dw[-i] = (np.dot(dz[-i], a[-i - 1].T) + dw[-i] * lambd) / m
            db[-i] = np.sum(dz[-i], axis=1, keepdims=True) / m
            da[-i - 1] = np.dot(w[-i].T, dz[-i])

        for i in range(1, L):
            w[i] = w[i] - alpha * dw[i]
            b[i] = b[i] - alpha * db[i]

    return cost, w[1:], b[1:]


def test(x, w, b, g):
    for i in range(len(w)):
        z = np.dot(w[i], x) + b[i]
        x = g[i](z)
    return x
