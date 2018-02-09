# -*- encoding:utf-8

import requests
import re

def get_location():
    res = requests.get('http://2017.ip138.com/ic.asp')

    left = res.text.find('charset=')
    if left >= 0:
        right = res.text[left:].find('"')
        if right >= 0:
            res.encoding = res.text[left+len('charset='):left+right].strip()

    r = re.compile(r'.*您的IP是：.* 来自：(?:.+[省州市区县镇组村])*(.+) (?:.+).*')
    for line in res.text.splitlines():
        m = r.match(line)
        if m:
            return m.group(1)

    return None
