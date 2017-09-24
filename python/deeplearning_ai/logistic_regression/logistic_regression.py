#!/usr/bin/python3
# -*- coding:utf-8

'logistic regression'

__author__ = 'tanhc'

import numpy as np

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def train(X, Y):
    nx, mx = np.shape(X)
    assert((1, mx) == np.shape(Y))
    alpha = 0.1

    w = np.zeros((nx, 1))
    b = 0.

    for i in range(1000):
        Z = np.dot(w.T, X) + b
        A = sigmoid(Z)
        dZ = A - Y
        dw = np.dot(X, dZ.T) / mx
        db = np.sum(dZ) / mx
        w = w - alpha * dw
        b = b - alpha * db

    return w, b

def test(x, w, b):
    z = np.dot(w.T, x) + b
    return sigmoid(z)


if __name__ == '__main__':
    #  from scipy import ndimage
    #
    #  nx = 128 * 128 * 3
    #
    #  cat, dog, otr = [], [], []
    #  for i in range(10):
    #      t = np.array(ndimage.imread('cat0%02d.jpg' % (i + 1), flatten=False))
    #      t = np.reshape(t, (nx, 1))
    #      cat.append(t)
    #  for i in range(5):
    #      t = np.array(ndimage.imread('dog0%02d.jpg' % (i + 1), flatten=False))
    #      t = np.reshape(t, (nx, 1))
    #      dog.append(t)
    #  for i in range(5):
    #      t = np.array(ndimage.imread('otr0%02d.jpg' % (i + 1), flatten=False))
    #      t = np.reshape(t, (nx, 1))
    #      otr.append(t)
    #
    #  X = cat[0]
    #  for i in range(1, 8):
    #      X = np.append(X, cat[i], axis=1)
    #  for i in range(4):
    #      X = np.append(X, dog[i], axis=1)
    #  for i in range(4):
    #      X = np.append(X, otr[i], axis=1)
    #
    #  Y = np.array([[1 if i < 8 else 0 for i in range(16)]])
    #
    #  w, b = train(X, Y)
    #  print(w, b)
    #
    #  print('cat:')
    #  for i in range(10):
    #      print(test(cat[i], 1., w, b))
    #
    #  print('dog:')
    #  for i in range(5):
    #      print(test(dog[i], 0., w, b))
    #
    #  print('otr:')
    #  for i in range(5):
    #      print(test(otr[i], 0., w, b))
    #

    a = np.loadtxt('diabetes_dataset/diabetes_train').T
    X = a[1:, :]
    Y = np.reshape(a[0, :], (1, 576))

    w, b = train(X, Y)
    print(w, b)

    print()
    print(test(X, w, b))
    print(Y)

    a = np.loadtxt('diabetes_dataset/diabetes_test').T
    X = a[1:, :]
    Y = np.reshape(a[0, :], (1, 192))

    print()
    print(test(X, w, b))
    print(Y)
