#!/usr/bin/python3
# -*- coding:utf-8

'deep neural networks'

__author__ = 'tanhuacheng'

import numpy as np
import time

def train(x, y, layers, activations, differentials, loss, dloss, alpha, lambd, iterations):
    assert(np.shape(y)[1] == np.shape(x)[1])
    assert(np.shape(y)[0] == layers[-1])
    assert(len(layers) == len(activations) == len(differentials))

    beta1 = 0.9
    beta2 = 0.999
    epsln = 1e-8

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
    vdw, vdb, sdw, sdb = ([0] for i in range(4))
    for i in range(1, L):
        w.append(np.random.randn(l[i], l[i - 1]) * np.sqrt(1 / l[i - 1]))
        b.append(np.random.randn(l[i], 1))
        z.append(None)
        a.append(None)

        da.append(0)
        dz.append(0)
        dw.append(0)
        db.append(0)

        vdw.append(0)
        vdb.append(0)
        sdw.append(0)
        sdb.append(0)

    #  idecay = 0
    #  decay = [[400, 5], [600, 0.5], [1000, 0.4], [7500, 0.5], [15000, 0.2], [-1]]
    #  decay = [[12500, 0.5], [-1]]

    cost = []
    tic = time.time()

    for it in range(iterations):
        if not (it % (iterations // 100)):
            percent = it // (iterations // 100)
            print('\rProceeding \033[32m[', end='')
            for i in range(10):
                print('=' if i < percent % 10 else ' ', end='')
            print(']\033[0m [% 3d%%] [%.2fs]' % (percent, time.time() - tic), end='', flush=True)

        for i in range(1, L):
            z[i] = np.dot(w[i], a[i - 1]) + b[i]
            a[i] = g[i](z[i])

        cost.append(np.sum(loss(a[-1], y)) / m)
        da[-1] = dloss(a[-1], y)

        for i in range(1, L):
            dz[-i] = da[-i] * d[-i](z[-i], a[-i])
            dw[-i] = np.dot(dz[-i], a[-i - 1].T) / m
            db[-i] = np.sum(dz[-i], axis=1, keepdims=True) / m
            da[-i - 1] = np.dot(w[-i].T, dz[-i])

            dw[-i] += w[-i] * lambd / m
            vdw[-i] = beta1 * vdw[-i] + (1 - beta1) * dw[-i]
            vdb[-i] = beta1 * vdb[-i] + (1 - beta1) * db[-i]
            sdw[-i] = beta2 * sdw[-i] + (1 - beta2) * np.power(dw[-i], 2)
            sdb[-i] = beta2 * sdb[-i] + (1 - beta2) * np.power(db[-i], 2)

        c_beta1 = 1 - np.power(beta1, it + 1)
        c_beta2 = 1 - np.power(beta2, it + 1)
        #  if it > 0 and (it % 1000) == 0:
        #      alpha = alpha * 0.95
        #  if it == decay[idecay][0]:
        #      alpha *= decay[idecay][1]
        #      idecay += 1

        for i in range(1, L):
            c_vdw = vdw[i] / c_beta1
            c_vdb = vdb[i] / c_beta1
            c_sdw = sdw[i] / c_beta2
            c_sdb = sdb[i] / c_beta2

            w[i] = w[i] - alpha * (c_vdw / (np.sqrt(c_sdw) + epsln))
            b[i] = b[i] - alpha * (c_vdb / (np.sqrt(c_sdb) + epsln))

    print('\rProceeding [----------] \033[1;32m[100%%]\033[0m [%.2fs]\n' % (time.time() - tic))

    return cost, w[1:], b[1:]


def test(x, w, b, g):
    for i in range(len(w)):
        z = np.dot(w[i], x) + b[i]
        x = g[i](z)
    return x
