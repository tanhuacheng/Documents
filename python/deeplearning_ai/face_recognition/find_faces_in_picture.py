#!/usr/bin/python3

from PIL import Image
import face_recognition

#  image = face_recognition.load_image_file('LinuxCon_Europe_Linus_Torvalds_03.jpg')
image = face_recognition.load_image_file('two.jpg')
pil_image = Image.fromarray(image)
face_locations = face_recognition.face_locations(image, model='cnn')
print('found {} face(s) in this photograph.'.format(len(face_locations)))

for face_location in face_locations:
    t,r,b,l = face_location
    print('A face is located at pixel location ({}, {}, {}, {})'.format(t, l, b, r))

    for x in range(l,r):
        pil_image.putpixel((x, t), (255, 0, 0))
        pil_image.putpixel((x, b), (255, 0, 0))
    for y in range(t, b):
        pil_image.putpixel((l, y), (255, 0, 0))
        pil_image.putpixel((r, y), (255, 0, 0))

pil_image.show()
