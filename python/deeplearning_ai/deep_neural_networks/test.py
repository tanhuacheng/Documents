#!/usr/bin/python3
# -*- coding:utf-8

'test for deep neural networks'

__author__ = 'tanhuacheng'

import numpy as np
from deep_neural_networks import train
from deep_neural_networks import test

def dtanh(x, y):
    return 1 - np.power(y, 2)

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def dsigmoid(x, y):
    return y * (1 - y)

def loss(x, y):
    x = np.maximum(x, 0.0000000001)
    x = np.minimum(x, 0.9999999999)
    return -(y * np.log(x) + (1 - y) * np.log(1 - x))

def dloss(x, y):
    x = np.maximum(x, 0.0000000001)
    x = np.minimum(x, 0.9999999999)
    return -y / x + (1 - y) / (1 - x)

def relu(x):
    return np.maximum(x, 0)

def drelu(x, y):
    return x > 0

import matplotlib.pyplot as plt

import time

def main():
    #  a = np.loadtxt('../logistic_regression/diabetes_dataset/diabetes_train').T
    #  x = a[1:]
    #  y = np.reshape(a[0, :], (1, np.shape(x)[1]))
    #
    #  a = np.loadtxt('../logistic_regression/diabetes_dataset/diabetes_test').T
    #  tx = a[1:]
    #  ty = np.reshape(a[0, :], (1, np.shape(tx)[1]))

    np.random.seed(0)
    x = np.random.randn(3, 8192)
    y = (np.sqrt(np.sum(np.power(x, 2), axis=0, keepdims=True)) < 1.7) * 1.0

    tx = np.random.randn(3, 1024)
    ty = (np.sqrt(np.sum(np.power(tx, 2), axis=0, keepdims=True)) < 1.7) * 1.0

    r = []
    for i in range(1, 2):
        for j in range(1, 2):
            np.random.seed(0)

            l = [9, 5, 1]
            g = [np.tanh, np.tanh, sigmoid]
            d = [dtanh, dtanh, dsigmoid]
            alpha = 0.0005
            lambd = 0
            it = 15000

            c, w, b = train(x, y, l, g, d, loss, dloss, alpha, lambd, it)
            cost = np.sum(c[-8:]) / 8
            wsum = 0
            for i in w:
                wsum += np.sqrt(np.sum(np.power(i, 2)))
            print(cost, wsum, l, alpha, lambd, it)

            t = [cost]

            result = (test(tx, w, b, g) > 0.5) - ty
            nerror = np.sum(np.abs(result)) * 100 / np.shape(ty)[1]
            t.append(nerror)
            print('%.2f%%' % nerror)

            result = (test(x, w, b, g) > 0.5) - y
            nerror = np.sum(np.abs(result)) * 100 / np.shape(y)[1]
            t.append(nerror)
            print('%.2f%%' % nerror)

            t.append(l)
            t.append(alpha)
            t.append(lambd)
            t.append(it)
            r.append(t)
            print()

    print()
    r.sort(key=lambda x: x[0])
    print(r[0:2])
    r.sort(key=lambda x: x[1])
    print(r[0:2])
    r.sort(key=lambda x: x[2])
    print(r[0:2])

    plt.plot(c)
    plt.show()

if __name__ == '__main__':
    main()
