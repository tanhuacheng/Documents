import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split

x_l = np.load('npy_dataset/X.npy')
y_l = np.load('npy_dataset/Y.npy')

X = np.concatenate((x_l[1034:1239], x_l[1239:1444]), axis=0)
Y = np.concatenate((np.zeros(205), np.ones(205)), axis=0).reshape(X.shape[0], 1)

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.15, random_state=42)
X_train_flatten = X_train.reshape(X_train.shape[0], X_train.shape[1]*X_train.shape[2])
X_test_flatten = X_test.reshape(X_test.shape[0], X_test.shape[1]*X_test.shape[2])

x_train = X_train_flatten.T
x_test = X_test_flatten.T
y_train = Y_train.T
y_test = Y_test.T
