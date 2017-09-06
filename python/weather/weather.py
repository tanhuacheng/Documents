#!/usr/bin/python3
# -*- coding:utf-8

'weather'

__author__ = 'tanhc'

import requests

def main ():
    req = requests.get('http://www.weather.com.cn/data/sk/101260101.html')
    res = eval(req.text.encode(req.encoding).decode('utf-8'))
    print(res)

if __name__ == '__main__':
    main()
