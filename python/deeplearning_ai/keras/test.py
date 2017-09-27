#!/usr/bin/python3
# -*- coding:utf-8

'test keras for DL for xor'

__author__ = 'tanhuacheng'

import numpy as np
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import SGD

def main():
    x = np.array([[0,0],[0,1],[1,0],[1,1]], 'float32')
    y = np.array([[0],[1],[1],[0]], 'float32')

    model = Sequential()
    model.add(Dense(2, input_dim=2, activation='tanh'))
    model.add(Dense(1, activation='sigmoid'))

    sgd = SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True)
    model.compile(loss='mean_squared_error', optimizer=sgd)

    history = model.fit(x, y, epochs=1000, batch_size=4, verbose=0)
    print(model.predict(x))


if __name__ == '__main__':
    main()
