#!/usr/bin/python3
# -*- coding:utf-8

'test for deep neural networks'

__author__ = 'tanhuacheng'

import numpy as np
from deep_neural_networks import train
from deep_neural_networks import test

def diff_tanh(x, y):
    return 1 - np.power(y, 2)

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def diff_sigmoid(x, y):
    return y * (1 - y)

def loss(x, y):
    return -(y * np.log(x) + (1 - y) * np.log(1 - x))

def diff_loss(x, y, z):
    return -y / x + (1 - y) / (1 - x)

def relu(x):
    return np.maximum(x, 0)

def diff_relu(x, y):
    return x > 0

import matplotlib.pyplot as plt

def main():
    import time
    a = np.loadtxt('../logistic_regression/diabetes_dataset/diabetes_train').T
    x = a[1:]
    y = np.reshape(a[0, :], (1, np.shape(x)[1]))


    np.random.seed(int(time.time()))
    alpha = 0.15
    l = [9, 16, 9, 1]

    tic = time.time()
    c, w, b = train(x, y, l,
                    [np.tanh, np.tanh, np.tanh, sigmoid],
                    [diff_tanh, diff_tanh, diff_tanh, diff_sigmoid],
                    loss, diff_loss, alpha, 50000)
    toc = time.time()

    c = np.reshape(c, (np.shape(c)[0],))
    print(toc - tic, l, alpha, np.sum(c[-8:]) / 8)
    #  print('\n', w, '\n', b)

    plt.plot(c)
    plt.show()


    result = (test(x, w, b, [np.tanh, np.tanh, np.tanh,sigmoid]) > 0.5) - y
    nerror = np.sum(np.abs(result))
    #  print(result)
    print(nerror, '%.2f%%' % (100 * nerror / np.shape(y)[1]))


    a = np.loadtxt('../logistic_regression/diabetes_dataset/diabetes_test').T
    x = a[1:]
    y = np.reshape(a[0, :], (1, np.shape(x)[1]))

    result = (test(x, w, b, [np.tanh, np.tanh, np.tanh,sigmoid]) > 0.5) - y
    nerror = np.sum(np.abs(result))
    #  print(result)
    print(nerror, '%.2f%%' % (100 * nerror / np.shape(y)[1]))

if __name__ == '__main__':
    main()
