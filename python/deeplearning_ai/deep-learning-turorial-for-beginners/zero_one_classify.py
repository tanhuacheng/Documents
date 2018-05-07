#!/usr/bin/python3

import numpy as np
import matplotlib.pyplot as plt
from zero_one_dataset import x_train, x_test, y_train, y_test


def initialize_weights_and_bias(dimension):
    w = np.full((dimension, 1), 0.01)
    b = 0.0
    return w, b

def sigmoid(z):
    return 1/(1+np.exp(-z))

def forward_backward_propagation(w, b, x_train, y_train):
    z = np.dot(w.T, x_train) + b
    y_head = sigmoid(z)
    loss = -y_train*np.log(y_head) - (1-y_train)*np.log(1-y_head)
    cost = (np.sum(loss) / x_train.shape[1])

    derivative_weight = np.dot(x_train, (y_head - y_train).T) / x_train.shape[1]
    derivative_bias = np.sum(y_head - y_train) / x_train.shape[1]

    gradients = {'derivative_weight': derivative_weight, 'derivative_bias': derivative_bias}

    return cost, gradients

def update(w, b, x_train, y_train, learning_rate, number_of_iterarion):
    cost_list = []
    cost_list2 = []
    index = []

    for i in range(number_of_iterarion):
        cost, gradients = forward_backward_propagation(w, b, x_train, y_train)
        cost_list.append(cost)
        w = w - learning_rate * gradients['derivative_weight']
        b = b - learning_rate * gradients['derivative_bias']

        if i % 5 == 0:
            cost_list2.append(cost)
            index.append(i)
            print('Cost after iteration %i: %f' % (i, cost))

    parameters = {'weight': w, 'bias': b}
    plt.plot(index, cost_list2)
    plt.xticks(index, rotation='vertical')
    plt.xlabel('Number of Iteration')
    plt.ylabel('Cost')
    plt.show()

    return parameters, gradients, cost_list

def predict(w, b, x_test):
    z = sigmoid(np.dot(w.T, x_test) + b)
    Y_prediction = np.zeros((1, x_test.shape[1]))
    for i in range(z.shape[1]):
        if z[0, i] <= 0.5:
            Y_prediction[0, i] = 0
        else:
            Y_prediction[0, i] = 1

    return Y_prediction

def logistic_regression(x_train, y_train, x_test, y_test, learning_rate, number_of_iterarion):
    w, b = initialize_weights_and_bias(x_train.shape[0])
    parameters, gradients, cost_list = update(w, b, x_train, y_train, learning_rate, number_of_iterarion)

    y_prediction_test = predict(parameters['weight'], parameters['bias'], x_test)
    y_prediction_train = predict(parameters['weight'], parameters['bias'], x_train)

    print('train accuracy: {} %'.format(100 - np.mean(np.abs(y_prediction_train - y_train)) * 100))
    print('test accuracy: {} %'.format(100 - np.mean(np.abs(y_prediction_test - y_test)) * 100))

    for i in range(y_train.shape[1]):
        if y_prediction_train[0,i] != y_train[0, i]:
            plt.imshow(x_train[:,i].reshape(64, 64))
            plt.show()
    print('xxx')

    for i in range(y_test.shape[1]):
        if y_prediction_test[0,i] != y_test[0, i]:
            plt.imshow(x_test[:,i].reshape(64, 64))
            plt.show()

logistic_regression(x_train, y_train, x_test, y_test, 0.02, 2000)
