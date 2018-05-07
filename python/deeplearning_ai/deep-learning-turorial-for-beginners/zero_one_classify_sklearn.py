#!/usr/bin/python3

import numpy as np
import matplotlib.pyplot as plt
from sklearn import linear_model
from zero_one_dataset import x_train, x_test, y_train, y_test

logreg = linear_model.LogisticRegression(random_state=42, max_iter=1500)
model = logreg.fit(x_train.T, y_train.T)
print('train accuracy: {}'.format(model.score(x_train.T, y_train.T)))
print('test accuracy: {}'.format(model.score(x_test.T, y_test.T)))
