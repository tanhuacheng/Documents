#!/usr/bin/python3
# -*- coding:utf-8

'NetEase API'

__version__ = '0.0.1'

import os
import binascii
import json
import base64
import hashlib
import requests

from Crypto.Cipher import AES
from http.cookiejar import LWPCookieJar

NET_EASE_NONCE   = '0CoJUm6Qyw8W8jud'
NET_EASE_PUBKEY  = '010001'
NET_EASE_MODULUS = '00e0b509f6259df8642dbc35662901477df22677ec152b5ff68ace615bb7' \
                   'b725152b3ab17a876aea8a5aa76d2e417629ec4ee341f56135fccf695280' \
                   '104e0312ecbda92557c93870114af6c9d05c4f7f0c3685b7a46bee255932' \
                   '575cce10b424d813cfe4875d3e82047b97ddef52741d546b8e289dc6935b' \
                   '3ece0462db0a22b8e7'
NET_EASE_AES_IV  = '0102030405060708'

NET_EASE_URL_LOGIN = 'https://music.163.com/weapi/login/cellphone'

COOKIE_PATH = os.path.join(os.path.curdir, '.cookies')

def create_secret_key():
    return binascii.hexlify(os.urandom(8))

def aes_encrypt(text, key):
    pad = 16 - len(text) % 16
    text += chr(pad) * pad
    encypter = AES.new(key, AES.MODE_CBC, NET_EASE_AES_IV)
    ciphertext = encypter.encrypt(text)
    return base64.b64encode(ciphertext).decode('utf-8')

def rsa_encrypt(text, pubkey, modulus):
    text = text[::-1]
    rs = pow(int(binascii.hexlify(text), 16), int(pubkey, 16), int(modulus, 16))
    return format(rs, 'x').zfill(256)

def encrypted_request(text):
    text = json.dumps(text)
    key = create_secret_key()
    encrypt_text = aes_encrypt(aes_encrypt(text, NET_EASE_NONCE), key)
    encrypt_key = rsa_encrypt(key, NET_EASE_PUBKEY, NET_EASE_MODULUS)
    return {'params': encrypt_text, 'encSecKey': encrypt_key}

class NetEase(object):

    def __init__(self):
        self.headers = {
            'Accept': '*/*',
            'Accept-Encoding': 'gzip, deflate, br',
            'Accept-Language': 'zh-CN,zh;q=0.9',
            'Connection': 'keep-alive',
            'Content-Type': 'application/x-www-form-urlencoded',
            'Host': 'music.163.com',
            'Referer': 'https://music.163.com/',
            'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko)' \
                          ' Ubuntu Chromium/62.0.3202.75 Chrome/62.0.3202.75 Safari/537.36'
        }

        self.session = requests.Session()
        self.session.cookies = LWPCookieJar(COOKIE_PATH)

    def login(self, phone, password, remember_login='true'):
        text = {
            'phone': phone,
            'password': hashlib.md5(password.encode()).hexdigest(),
            'rememberLogin': remember_login
        }
        data = encrypted_request(text)

        conn = self.session.post(NET_EASE_URL_LOGIN, data=data, headers=self.headers)
        self.session.cookies.save()

        conn.encoding = 'UTF-8'

        return json.loads(conn.text)

if __name__ == '__main__':
    import sys
    from getpass import getpass

    phone = input('phone number: ')
    password = getpass('[%s] password for %s: ' % (sys.argv[0], phone));

    print(NetEase().login(phone, password))
