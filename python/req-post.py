#!/usr/bin/python3
# -*- coding:utf-8

'a test module'

__author__ = 'tanhc'

import requests

def test ():
    url = 'https://ir2.co:8443/pushsvr'
    pay = {
        # 'token' : 'HW_0865164028066403300000673100CN01',
        # 'token' : 'HW_0868029025709445300000673100CN01',
        'token' : 'MI_Kv/KPH7Kzx4eQ0mb/7j8sNPxn1irCUQkj/u/O4IxtT0=',
        'msg' : '456'
    }

    r = requests.post(url, data = pay)
    print(r.text)

if __name__ == '__main__':
    test()
