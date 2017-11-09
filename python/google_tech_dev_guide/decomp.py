#!/usr/bin/python3
# -*- coding:utf-8

''' decompression string '''

__author__ = 'tanhuacheng'

def find_most_pair(s):
    x = 0
    p = 0

    for i in range(len(s)):
        if s[i] == '[':
            x = i if not p else x
            p += 1
        elif s[i] == ']':
            p -= 1
            if not p:
                return x, i

    return -1, -1

def decomp(s):
    print(s, ':')
    t = s
    r = ''
    i = 0

    while len(t):
        if str.isdigit(t[i]):
            i += 1
            continue
        elif t[i] == '[':
            x = find_most_pair(t)[1]
            r, t = r + int(t[0:i]) * decomp(t[i + 1:x]), t[x + 1:]
        else:
            r, t = r + t[i], t[i + 1:]
        i = 0

    print(r)
    return r

def decompress(text, start=0, repeat=1):
    for _ in range(repeat):
        i = start
        while i < len(text) and text[i] != ']':
            if text[i].islower():
                yield text[i]
            else:
                times = 0
                while text[i].isdigit():
                    times = times * 10 + int(text[i])
                    i += 1
                i += 1
                for item in decompress(text, i, times):
                    if isinstance(item, str):
                        yield item
                    else:
                        i = item
            i += 1

    if start > 0:
        yield i

if __name__ == '__main__':
    decomp('3[a]b')
    decomp('3[a4[b]]c')
    decomp('b3[a4[b]]c')

    text = 'b3[a4[bc]d]ee'
    print('\n' + text)
    for item in decompress(text):
        print(item, end='')
    print()
