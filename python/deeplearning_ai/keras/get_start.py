#!/usr/bin/python3
# -*- coding:utf-8

import numpy as np
from keras.models import Sequential
from keras.layers import Dense
from keras import losses

x_train = np.loadtxt('../logistic_regression/diabetes_dataset/diabetes_train')
y_train = x_train[:,0].reshape(x_train.shape[0], 1)
x_train = x_train[:,1:];

model = Sequential()
model.add(Dense(units=9, activation='tanh', input_dim=x_train.shape[1]))
model.add(Dense(units=16, activation='tanh'))
model.add(Dense(units=9, activation='tanh'))
model.add(Dense(units=1, activation='sigmoid'))
model.compile(loss='mean_squared_error', optimizer='sgd', metrics=['accuracy'])

model.fit(x_train, y_train, epochs=80000, batch_size=32)
predict = model.predict(x_train[32:64, :]).T
print(np.append(predict.reshape(1,32), y_train[32:64,:].reshape(1,32), axis=0))
