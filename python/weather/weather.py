#!/usr/bin/python3
# -*- coding:utf-8

"""
查询城市天气

Usage:
    weather.py <city>

Options:
    -h,--help   显示帮助菜单

Example:
    weather.py 兴义
"""

__author__ = 'tanhuacheng'

import requests
from docopt import docopt
try:
    import xml.etree.cElementTree as ET
except ImportError:
    import xml.etree.ElementTree as ET

def main ():
    arguments = docopt(__doc__)
    req = requests.get('http://wthrcdn.etouch.cn/WeatherApi?city=%s' % arguments['<city>'])
    print(req.text)

if __name__ == '__main__':
    main()
