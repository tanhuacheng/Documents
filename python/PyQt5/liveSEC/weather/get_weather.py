# -*- coding:utf-8

import requests
import xml.etree.ElementTree as ElementTree

def __xmltodict(xml):
    res = {}
    for elem in xml:
        res[elem.tag] = elem.text if elem.text else __xmltodict(elem)
    return res

def get_weather(city):
    res = requests.post('http://wthrcdn.etouch.cn/WeatherApi?city=%s' % city)
    xml = ElementTree.fromstring(res.text)

    res = {}
    for elem in xml:
        if elem.tag == 'forecast' or elem.tag == 'zhishus':
            res[elem.tag] = []
            for e in elem:
                res[elem.tag].append(__xmltodict(e))
        else:
            res[elem.tag] = elem.text if elem.text else __xmltodict(elem)
    return res
