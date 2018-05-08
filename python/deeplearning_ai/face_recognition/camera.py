#!/usr/bin/python3

import cv2
import numpy as np
import matplotlib.pyplot as plot
import face_recognition

cap = cv2.VideoCapture(0)
while True:
    ret, frame = cap.read()

    face_locations = face_recognition.face_locations(frame)#, model='cnn')
    for face_location in face_locations:
        t,r,b,l = face_location
        r,b=r-1,b-1
        frame[t,l:r] = (255,0,0)
        frame[b,l:r] = (255,0,0)
        frame[t:b,l] = (255,0,0)
        frame[t:b,r] = (255,0,0)

    cv2.imshow('capture', frame)
    if (cv2.waitKey(1) & 0xff) == ord('q'):
        break

cap.release()
cv2.destroyAllWindows()
