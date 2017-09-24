#!/usr/bin/python3
# -*- coding:utf-8

'deep neural networks'

__author__ = 'tanhuacheng'

import numpy as np

def train(x, y, layers, activation, differential, loss, diff_loss, alpha, iterations):
    w = [None]
    b = [None]
    z = [None]
    a = [x]

    l, g, d, m = layers.copy(), activation.copy(), differential.copy(), np.shape(x)[1]
    l.insert(0, np.shape(x)[0])
    g.insert(0, None)
    d.insert(0, None)

    L = len(l)
    assert(L == len(g) == len(d))

    da, dz, dw, db = [None], [None], [None], [None]
    for i in range(1, L):
        w.append(np.random.randn(l[i], l[i - 1]) * 1)
        b.append(np.zeros((l[i], 1)))
        z.append(None)
        a.append(None)

        da.append(None)
        dz.append(None)
        dw.append(None)
        db.append(None)

    cost = []
    for it in range(iterations):
        for i in range(1, L):
            z[i] = np.dot(w[i], a[i - 1]) + b[i]
            a[i] = g[i](z[i])

        loss_y = loss(a[-1], y)
        if it > iterations - 100000:
            cost.append(np.sum(loss_y, axis=1, keepdims=True) / m)
            if cost[-1] < 0.0025:
                print(cost[-1])
                return cost, w[1:], b[1:]
        da[-1] = diff_loss(a[-1], y, loss_y)

        for i in range(1, L):
            dz[-i] = da[-i] * d[-i](z[-i], a[-i])
            dw[-i] = np.dot(dz[-i], a[-i - 1].T) / m
            db[-i] = np.sum(dz[-i], axis=1, keepdims=True) / m
            da[-i - 1] = np.dot(w[-i].T, dz[-i])

        #  if it > 10000 and (it % 1000) == 0:
        #      alpha = alpha * 0.8

        for i in range(1, L):
            w[i] = w[i] - alpha * dw[i]
            b[i] = b[i] - alpha * db[i]

    return cost, w[1:], b[1:]


def test(x, w, b, g):
    for i in range(len(w)):
        z = np.dot(w[i], x) + b[i]
        x = g[i](z)
    return x


if __name__ == '__main__':
    pass
