#!/usr/bin/python3
# -*- coding:utf-8

'net ease webapi'

__author__ = 'tanhuacheng'

import os
import binascii
import json
import base64
import requests
import hashlib

from Crypto.Cipher import AES
from http.cookiejar import LWPCookieJar

NET_EASE_URL = 'https://music.163.com/weapi/login/cellphone'
NONCE        = '0CoJUm6Qyw8W8jud'
PUBKEY       = '010001'
MODULUS      = ('00e0b509f6259df8642dbc35662901477df22677ec152b5ff68ace615bb7'
                'b725152b3ab17a876aea8a5aa76d2e417629ec4ee341f56135fccf695280'
                '104e0312ecbda92557c93870114af6c9d05c4f7f0c3685b7a46bee255932'
                '575cce10b424d813cfe4875d3e82047b97ddef52741d546b8e289dc6935b'
                '3ece0462db0a22b8e7')

def create_secret_key():
    return binascii.hexlify(os.urandom(8))

def aes_encrypt(text, key):
    pad = 16 - len(text) % 16
    text += chr(pad) * pad
    encypter = AES.new(key, 2, '0102030405060708')
    ciphertext = encypter.encrypt(text)
    ciphertext = base64.b64encode(ciphertext).decode('utf-8')
    return ciphertext

def rsa_encrypt(text, pubkey, modulus):
    text = text[::-1]
    rs = pow(int(binascii.hexlify(text), 16), int(pubkey, 16), int(modulus, 16))
    #  rs = int(text, 16)**int(pubkey, 16)%int(modulus, 16)
    return format(rs, 'x').zfill(256)

def encrypted_request(text):
    text = json.dumps(text)
    key = create_secret_key()
    encrypt_text = aes_encrypt(aes_encrypt(text, NONCE), key)
    encrypt_key = rsa_encrypt(key, PUBKEY, MODULUS)
    return {'params': encrypt_text, 'encSecKey': encrypt_key}

class NetEase(object):

    def __init__(self):
        self.header = {
            'Accept': '*/*',
            'Accept-Encoding': 'gzip, deflate, br',
            'Accept-Language': 'zh-CN,zh;q=0.9',
            'Connection': 'keep-alive',
            'Content-Type': 'application/x-www-form-urlencoded',
            'Host': 'music.163.com',
            'Referer': 'https://music.163.com/',
            'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/62.0.3202.75 Chrome/62.0.3202.75 Safari/537.36'
        }

        self.cookie_path = os.path.join(os.path.curdir, '.cookies')
        self.session = requests.Session()
        self.session.cookies = LWPCookieJar(self.cookie_path)

    def phone_login(self, phone, password, remember_login='true'):
        text = {
            'phone': phone,
            'password': hashlib.md5(password.encode()).hexdigest(),
            'rememberLogin': remember_login
        }
        conn = self.session.post(NET_EASE_URL, data=encrypted_request(text), headers=self.header)
        self.session.cookies.save()

        print('conn:', conn)
        print('conn.text:', conn.text)

        return conn.text


if __name__ == '__main__':
    import sys
    from getpass import getpass

    phone = input('phone number: ')
    password = getpass('[%s] password for %s: ' % (sys.argv[0], phone));

    net_ease = NetEase()
    net_ease.phone_login(phone, password)
