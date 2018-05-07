#!/usr/bin/python3

import numpy as np
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import cross_val_score
from keras.models import Sequential
from keras.layers import Dense
from zero_one_dataset import x_train, x_test, y_train, y_test

def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(units=8, kernel_initializer='uniform', activation='relu',
        input_dim=x_train.shape[0]))
    classifier.add(Dense(units=4, kernel_initializer='uniform', activation='tanh'))
    classifier.add(Dense(units=1, kernel_initializer='uniform', activation='sigmoid'))
    classifier.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])
    return classifier

#  classifier = KerasClassifier(build_fn=build_classifier, epochs=100)
#  accuracies = cross_val_score(estimator=classifier, X=x_train.T, y=y_train.T, cv=2)
#  mean = accuracies.mean()
#  variance = accuracies.std()
#
#  print()
#  print('Accuracy mean:', str(mean))
#  print('Accuracy variance', str(variance))

classifier = build_classifier()
history = classifier.fit(x_train.T, y_train.T, epochs=100, batch_size=8)

y_prediction_test=classifier.predict(x_test.T).T
for i in range(y_prediction_test.shape[1]):
    if y_prediction_test[0,i] <= 0.5:
        y_prediction_test[0,i] = 0
    else:
        y_prediction_test[0,i] = 1

print('test accuracy: {} %'.format(100 - np.mean(np.abs(y_prediction_test - y_test)) * 100))
