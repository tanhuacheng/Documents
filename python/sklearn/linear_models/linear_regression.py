#!/usr/bin/python3

import matplotlib.pyplot as plt
import numpy as np
from sklearn import datasets, linear_model
from sklearn.metrics import mean_squared_error, r2_score

diabetes = datasets.load_diabetes()

diabetes_X = diabetes.data[:, np.newaxis, 2]

diabetes_X_train = diabetes_X[:-20]
diabetes_X_test = diabetes_X[-20:]

diabetes_Y_train = diabetes.target[:-20]
diabetes_Y_test = diabetes.target[-20:]

#  regr = linear_model.LinearRegression()
regr = linear_model.Ridge(alpha=0.2)
regr.fit(diabetes_X_train, diabetes_Y_train)
diabetes_Y_pred = regr.predict(diabetes_X_test)

print('Coefficients:\n', regr.coef_)
print('Mean squred error: %.2f' % mean_squared_error(diabetes_Y_test, diabetes_Y_pred))
print('Variance score: %.2f' % r2_score(diabetes_Y_test, diabetes_Y_pred))

plt.scatter(diabetes_X_test, diabetes_Y_test)
plt.plot(diabetes_X_test, diabetes_Y_pred)
plt.xticks(())
plt.yticks(())
plt.show()
