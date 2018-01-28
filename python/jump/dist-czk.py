#!/usr/bin/python3
# -*- coding:utf-8

import os
import sys
from PIL import Image
import matplotlib.pyplot as plt
import math


def color_diff(p1, p2):
    return math.sqrt(sum(map(lambda x1, x2: (x1 - x2)**2, p1[:3], p2[:3])))

def show_pixel(img, x, y, n=5, color=(255,0,0)):
    for i in range(n):
        xx = 0 if x - i < 0 else x - i
        for j in range(n):
            yy = 0 if y - j < 0 else y - j
            img.putpixel((xx, yy), color)

def is_token(p):
    token_c = (54, 60, 102)
    return color_diff(p, token_c) <= 5

def is_shadow(p):
    shadows = (
        (178, 172, 114),
        (131, 136, 169),
        (140, 156, 170),
        (148, 166, 163),
        (178, 149, 101),)

    for shadow in shadows:
        if color_diff(p, shadow) <= 10:
            return True

    return False


if __name__ == '__main__':
    img1 = Image.open(os.path.join(os.path.join(os.path.curdir, 'screen'), 'screen2.png'))
    img2 = Image.open(os.path.join(os.path.join(os.path.curdir, 'screen'), 'screen2.png'))
    w, h = img1.size

    locate_token_x, locate_token_y = 0, 0
    find_flag = 0
    for y in range(h//3, h, 40):
        for x in range(0, w, 10):
            img2.putpixel((x, y), (0, 255, 0))
            p = img1.getpixel((x, y))
            token_c = (54, 60, 102)
            if color_diff(p, token_c) <= 80:
                find_flag = 1
                break
        if find_flag:
            break

    token_x, token_y = 0, 0
    board_x, board_y = 0, 0
    token_xs = []

    for i in range(y-100, y+500 if y+500 < h else h):
        token_l, token_r = 0, 0

        for j in range(x-50, x+200 if x+200 < w else w):
            p = img1.getpixel((j, i))
            if is_token(p):
                token_l = j
                token_y = i
                break
        else:
            if token_y:
                break

        if token_l:
            for k in range(0, w, 2):
                j = w - k - 1
                if j < token_l:
                    break
                p = img1.getpixel((j, i))
                if is_token(p):
                    token_r = j
                    break

        if token_r:
            token_xs.append((token_l + token_r) / 2)

    if token_y:
        token_x = int(sum(token_xs)/len(token_xs) + 4)
        token_y = token_y - 15
    else:
        #  print('没有找到小人')
        sys.exit()

    for xj in range(token_x-50,token_x+50):
        show_pixel(img2, xj, token_y-200, 8)
        show_pixel(img2, xj, token_y+30, 8)
    for yi in range(token_y-200,token_y+30):
        show_pixel(img2, token_x-50, yi, 8)
        show_pixel(img2, token_x+50, yi, 8)


    show_pixel(img2, token_x, token_y, 10)
    plt.imshow(img2)
    #  plt.show()
    #  sys.exit()


    last_board_l, last_board_r = 0, 0

    board_w = 0
    width_cnt = 0
    for i in range(0, h, 2):
        if i < h/3:
            continue
        if i > 2*h/3:
            break

        board_l, board_r = 0, 0

        x = w - 1
        base = img1.getpixel((w - 1, i))
        for j in range(5, w, 2):
            x = w - j - 1;
            if abs(x - token_x) < 45:
                continue
            p = img1.getpixel((x, i))
            if color_diff(p, base) > 20:
                show_pixel(img2, x, i)
                board_r = x
                break

        if board_r:
            base = img1.getpixel((0, i))
            x = x - 5
            for k in range(0, x, 2):
                j = x - k - 1
                if abs(j - token_x) < 45:
                    continue

                p = img1.getpixel((j, i))
                if color_diff(p, base) <= 20 or is_shadow(p):
                    show_pixel(img2, j, i)
                    board_l = j
                    break

        if board_l:
            if not board_x:
                board_x = (board_l + board_r)/2
            else:
                board_x = 0.8*board_x + 0.2*((board_l + board_r)/2)

            if last_board_l and last_board_r and (abs(board_l - last_board_l) > 40 or
                    abs(board_r - last_board_r) > 40):
                break

            last_board_l, last_board_r = board_l, board_r

            width = board_r - board_l + 1
            if width > board_w + 20:
                board_w = width
                width_cnt = 0
            else:
                board_y = i
                width_cnt += 1
                if width_cnt == 15:
                    break

    board_x = int(board_x)
    board_y = board_y - 15

    #  show_pixel(img2, token_x, token_y, 10)
    show_pixel(img2, board_x, board_y, 10)

    plt.imshow(img2)
    plt.show()

    dist = math.sqrt((token_x - board_x)**2 + (token_y - board_y)**2)
    print(int(dist*1.383))
