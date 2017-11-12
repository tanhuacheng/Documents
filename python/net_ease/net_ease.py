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
import time

from Crypto.Cipher import AES
from http.cookiejar import LWPCookieJar

class NetEase(object):

    def __init__(self, configs_dir=None):
        self.__headers = {
            'Accept': '*/*',
            'Accept-Encoding': 'gzip, deflate, br',
            'Accept-Language': 'zh-CN,zh;q=0.9',
            'Connection': 'keep-alive',
            'Content-Type': 'application/x-www-form-urlencoded',
            'Host': 'music.163.com',
            'Origin': 'https://music.163.com',
            'Referer': 'https://music.163.com/',
            'User-Agent':
                'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) ' \
                'Ubuntu Chromium/62.0.3202.75 Chrome/62.0.3202.75 Safari/537.36'
        }

        self.__nonce   = '0CoJUm6Qyw8W8jud'
        self.__pubkey  = '010001'
        self.__iv      = '0102030405060708'
        self.__modulus = '00e0b509f6259df8642dbc35662901477df22677ec152b5ff68ace615bb7' \
                         'b725152b3ab17a876aea8a5aa76d2e417629ec4ee341f56135fccf695280' \
                         '104e0312ecbda92557c93870114af6c9d05c4f7f0c3685b7a46bee255932' \
                         '575cce10b424d813cfe4875d3e82047b97ddef52741d546b8e289dc6935b' \
                         '3ece0462db0a22b8e7'

        self.__uri_login           = 'https://music.163.com/weapi/login/cellphone'
        self.__uri_recommend       = 'https://music.163.com/weapi/v2/discovery/recommend/songs'
        self.__uri_playlist        = 'https://music.163.com/weapi/user/playlist'
        self.__uri_playlist_detail = 'https://music.163.com/weapi/v3/playlist/detail'
        self.__uri_musics          = 'https://music.163.com/weapi/song/enhance/player/url'

        self.__default_timeout = 5


        self.__phone = None
        self.__password = None

        self.__session = requests.Session()
        self.__login = {}

        self.__path_cookies = None
        self.__path_login = None

        self.__recommend = {}

        if (not isinstance(configs_dir, str)) or (not len(configs_dir)):
            return
        try:
            if not os.path.isdir(configs_dir):
                os.mkdir(configs_dir)
        except Exception as e:
            print('NetEase.__init__(W): {0}'.format(e))
            return

        self.__path_cookies = os.path.join(configs_dir, 'cookies')
        self.__path_login = os.path.join(configs_dir, 'login')

        self.__session.cookies = LWPCookieJar(self.__path_cookies)
        try:
            self.__session.cookies.load()
            for c in self.__session.cookies:
                break
            else:
                self.__session.cookies.clear()
        except:
            pass

        try:
            with open(self.__path_login, 'r') as f:
                self.__login = json.load(f)
                self.__phone = self.__login['my_phone']
                self.__password = self.__login['my_password']
        except:
            pass

        if (not self.__login) or (not self.__phone) or (not self.__password):
            self.__phone = None
            self.__password = None
            self.__session.cookies.clear()
            self.__login = {}

        self.__login_if_needed()

    def __create_seckey(self):
        return binascii.hexlify(os.urandom(8))

    def __aes_encrypt(self, text, seckey):
        pad = 16 - len(text) % 16
        if pad:
            text += chr(pad) * pad

        encypter = AES.new(seckey, AES.MODE_CBC, self.__iv)
        enc_text = encypter.encrypt(text)
        enc_text = base64.b64encode(enc_text).decode('utf-8')

        return enc_text

    def __rsa_encrypt(self, text):
        text = text[::-1]
        rsa = pow(int(binascii.hexlify(text), 16), int(self.__pubkey, 16), int(self.__modulus, 16))
        rsa = format(rsa, 'x').zfill(256)

        return rsa

    def __encrypted_request(self, text):
        text = json.dumps(text)
        seckey = self.__create_seckey()

        encrypt_txt = self.__aes_encrypt(self.__aes_encrypt(text, self.__nonce), seckey)
        encrypt_key = self.__rsa_encrypt(seckey)

        data = {'params': encrypt_txt, 'encSecKey': encrypt_key}

        return data

    def __save(self):
        try:
            if self.__path_cookies:
                self.__session.cookies.save()
            if self.__path_login:
                with open(self.__path_login, 'w') as f:
                    json.dump(self.__login, f)
        except Exception as e:
            print('NetEase.__save(W): {0}'.format(e))

    def __login_if_needed(self, timeout=5):
        if (not self.__phone) or (not self.__password):
            return False

        if self.__login:
            for c in self.__session.cookies:
                return True

        self.__session.cookies.clear()
        self.__login = {}
        self.__save()

        text = {
            'phone': self.__phone,
            'password': hashlib.md5(self.__password.encode()).hexdigest(),
            'rememberLogin': 'true'
        }

        try:
            r = self.__session.post(self.__uri_login, data=self.__encrypted_request(text),
                                    headers=self.__headers, timeout=self.__default_timeout)
            r.raise_for_status()
            r.encoding = 'utf-8'
            r = json.loads(r.text)
        except Exception as e:
            print('NetEase.__login_if_needed(E): {0}'.format(e))
            return False

        if not 'code' in r or r['code'] < 200 or r['code'] > 299:
            print('NetEase.__login_if_needed(E): {0}'.format(r['msg'] if 'msg' in r else 'Unknown error'))
            return False

        r['my_phone'] = self.__phone
        r['my_password'] = self.__password
        self.__login = r
        self.__save()

        self.__recommend = {}

        print('NetEase.__login_if_needed(I): user successfully login')

        return True

    def login(self, phone, password):
        self.__phone = phone
        self.__password = password
        self.__session.cookies.clear()
        self.__login = {}

        result = self.__login_if_needed()

        return result

    def is_login(self):
        return self.__login_if_needed()

    def __post(self, uri, req, timeout=5):
        for c in self.__session.cookies:
            if c.name == '__csrf':
                uri, csrf = uri + '?csrf_token=' + c.value, c.value
                break
        else:
            return None

        req = req.copy()
        req['csrf_token'] = csrf
        req = self.__encrypted_request(req)

        try:
            r = self.__session.post(uri, data=req, headers=self.__headers, timeout=timeout)
            r.raise_for_status()
            r.encoding = 'utf-8'
            r = json.loads(r.text)
        except Exception as e:
            return None

        if not 'code' in r or r['code'] < 200 or r['code'] > 299:
            return None

        return r

    def get_recommend(self):
        if self.__recommend:
            # update in 6:00 AM everyday
            sec6hour = 6 * 60 * 60
            day_save = time.localtime(self.__recommend['time'] - sec6hour)
            day_curr = time.localtime(time.time() - sec6hour)
            if day_save.tm_year == day_curr.tm_year and day_save.tm_yday == day_curr.tm_yday:
                return self.__recommend['recommend'].copy()

        req = {'offset': 0, 'total': True, 'limit': 30}
        r = self.__post(self.__uri_recommend, req, timeout=self.__default_timeout)
        if not r:
            return None

        r['time'] = time.time()
        self.__recommend = r

        print('NetEase.get_recommend(I): recommend refreshed')

        return r['recommend'].copy()

    def get_playlists(self):
        if not self.__login:
            return None

        req = {'offset': 0, 'uid': self.__login['profile']['userId'], 'limit': 999}
        r = self.__post(self.__uri_playlist, req, timeout=self.__default_timeout)
        if not r:
            return None

        print('NetEase.get_playlists(I): playlists refreshed')

        return r['playlist']

    def get_playlist_detail(self, playlist_id, n=999):
        req = {'offset': 0, 'total': True, 'id': playlist_id, 'limit': 999, 'n': n}
        r = self.__post(self.__uri_playlist_detail, req, timeout=self.__default_timeout)
        if not r:
            return None

        return r['playlist']['tracks']

    def get_musics_url(self, music_ids, bit_rate=320000):
        req = {'ids': music_ids, 'br': bit_rate}
        r = self.__post(self.__uri_musics, req, timeout=self.__default_timeout)
        if not r:
            return None

        return r['data']


if __name__ == '__main__':
    import sys
    import vlc
    import readline
    from getpass import getpass

    net_ease = NetEase(os.path.curdir)
    if not net_ease.is_login():
        phone = input('phone number: ')
        password = getpass('[%s] password for %s: ' % (sys.argv[0], phone))
        net_ease.login(phone, password)

    help_str = '''
commands:
    h,  help                display this comment
    l,  list                display play list
    l,  list <id>           display play list detail pointer by id
    p,  play <id>           play song pointer by id
    pn, pnext               play next song
    pp, pprev               play prev song
    P,  pause               pause playing music
    r,  resume              resume paused music
    s,  stop                stop playing music
    v+                      increase volume by 1 step
    v++                     increase volume by 10 step
    v-                      decrease volume by 1 step
    v--                     decrease volume by 10 step
    v=                      display current volume
    q,  quit
    '''
    print(help_str)

    playlist = None
    player = None
    mid = -999

    while True:
        try:
            cmd = input('> ').strip()
        except:
            sys.exit()
        if not len(cmd):
            continue

        mid_prev = mid

        if cmd == 'help' or cmd == 'h':
            print(help_str)

        elif cmd == 'list' or cmd == 'l':
            recommends = net_ease.get_recommend()
            playlist_details = []
            playlists = net_ease.get_playlists()
            for l in playlists:
                playlist_details.append(net_ease.get_playlist_detail(l['id'], l['trackCount']))

            for i in range(len(playlists) + 1):
                if i < len(playlists):
                    name = playlists[i]['name']
                    count = playlists[i]['trackCount']
                else:
                    name = '每日推荐'
                    count = len(recommends)
                pad = 50 - len(name)
                for s in name:
                    if ord(s) > int('4dff', 16) and ord(s) < int('9fa6', 16):
                        pad -= 1
                print('% 4d' % i, name, ' ' * pad, count)

        elif cmd.startswith('list') or (cmd[0] == 'l' and len(cmd) > 1 and cmd[1].isspace()):
            i = int(cmd[2:]) if cmd[1].isspace() else int(cmd[4:])
            if i < 0 or i > len(playlist_details) + 1:
                print('invalid param')
            playlist = playlist_details[i] if i < len(playlist_details) else recommends
            if player:
                player.release()
                player = None

            for i in range(len(playlist)):
                end = '\n'
                if not i % 2:
                    pad = 50 - len(playlist[i]['name'])
                    for s in playlist[i]['name']:
                        if ord(s) > int('4dff', 16) and ord(s) < int('9fa6', 16):
                            pad -= 1
                    end = ' ' * pad
                print('% 4d' % i, playlist[i]['name'], end=end)
            if len(playlist) % 2:
                print()

        elif cmd.startswith('play') or (cmd[0] == 'p' and len(cmd) > 1 and cmd[1].isspace()):
            mid = int(cmd[2:]) if cmd[1].isspace() else int(cmd[4:])

        elif cmd == 'pn' or cmd == 'pnext':
            mid = 0 if mid == -9999 else mid + 1

        elif cmd == 'pp' or cmd == 'pprev':
            mid = 0 if mid == -9999 else mid - 1

        elif cmd == 'pause' or cmd == 'P':
            if player and player.is_playing() == 1:
                player.pause()

        elif cmd == 'resume' or cmd == 'r':
            if player and player.is_playing != 1:
                player.play()

        elif cmd == 'stop' or cmd == 's':
            if player:
                player.release()
                player = None

        elif cmd == 'v+':
            if player:
                vol = player.audio_get_volume() + 1
                player.audio_set_volume(vol)
                print('vol:', vol)

        elif cmd == 'v++':
            if player:
                vol = player.audio_get_volume() + 10
                player.audio_set_volume(vol)
                print('vol:', vol)

        elif cmd == 'v-':
            if player:
                vol = player.audio_get_volume() - 1
                player.audio_set_volume(vol)
                print('vol:', vol)

        elif cmd == 'v--':
            if player:
                vol = player.audio_get_volume() - 10
                player.audio_set_volume(vol)
                print('vol:', vol)

        elif cmd == 'v=':
            if player:
                print('vol:', player.audio_get_volume())

        elif cmd == 'quit' or cmd == 'q':
            sys.exit()

        else:
            print('command not found')


        # paly
        if not playlist:
            mid = mid_prev
            continue

        if mid != mid_prev:
            mid = mid % len(playlist)
            if mid == mid_prev:
                print('same song[{0}]:\nname={1}\nurl={2}'.format(mid, playlist[mid]['name'], url))
                continue

            if player:
                player.release()
                player = None

            url = net_ease.get_musics_url([playlist[mid]['id']])[0]['url']
            player = vlc.MediaPlayer(url)
            if not player:
                print('can not create player')
                mid = mid_prev
                continue
            if player.play() == 0:
                print('start play[{0}]:\nname={1}\nurl={2}'.format(mid, playlist[mid]['name'], url))
            else:
                print('faild play[{0}]:\nname={1}\nurl={2}'.format(mid, playlist[mid]['name'], url))
                mid = mid_prev
