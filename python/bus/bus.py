#!/usr/bin/python3
# -*- coding:utf-8

url='https://ditu.amap.com/service/poiInfo?query_type=TQUERY&p'

import requests

r = requests.get(url)
print(r)
print(r.text)
