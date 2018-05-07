#!/usr/bin/python3

import os
import numpy as np
import imageio
import scipy
from keras.utils import to_categorical
from sklearn.model_selection import train_test_split

def get_img(data_path, img_size, grayscale_images):
    img = imageio.imread(data_path)
    img = img if not grayscale_images else np.dot(img, [0.299, 0.587, 0.114])
    return scipy.misc.imresize(img, (img_size, img_size, 1 if grayscale_images else 3))

def get_dataset(dataset_path='Sign-Language-Digits-Dataset/Dataset',
        img_size=64, grayscale_images=True, test_size=0.2):
    try:
        X = np.load('npy_dataset/X.npy')
        Y = np.load('npy_dataset/Y.npy')
    except:
        X = []
        Y = []
        for label in os.listdir(dataset_path):
            datas_path = dataset_path + '/' + label
            for data in os.listdir(datas_path):
                X.append(get_img(datas_path + '/' + data, img_size, grayscale_images))
                Y.append(int(label))

        X = 1 - np.array(X).astype('float32') / 255
        Y = np.array(Y).astype('float32')
        Y = to_categorical(Y, 10)
        if not os.path.exists('npy_dataset'):
            os.makedirs('npy_dataset')
        np.save('npy_dataset/X.npy', X)
        np.save('npy_dataset/Y.npy', Y)

        return train_test_split(X, Y, test_size=test_size, random_state=42)

if __name__ == '__main__':
    get_dataset()
