#!/usr/bin/python3
# -*- coding:utf-8

'weather'

__author__ = 'tanhc'

import requests

def main ():
    req = requests.get('http://www.weather.com.cn/data/sk/101190408.html')
    print(req.text)

if __name__ == '__main__':
    main()
