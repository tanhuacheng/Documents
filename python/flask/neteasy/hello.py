#!/usr/bin/python3
# -*- coding:utf-8

import os, sys
from flask import Flask, render_template
import net_ease
from getpass import getpass

app = Flask(__name__)
ne = net_ease.NetEase(os.path.curdir)

if not ne.is_login():
    phone = input('phone number: ')
    password = getpass('[%s] password for %s: ' % (sys.argv[0], phone))
    ne.login(phone, password)

recommends = ne.get_recommend()

playlist_details = []
playlists = ne.get_playlists()
for l in playlists:
    playlist_details.append(ne.get_playlist_detail(l['id'], l['trackCount']))

@app.route('/')
def index():
    albums = []

    album = { 'name': '每日推荐', 'musics': [] }
    for r in recommends:
        music = {}
        music['name'] = r['name']
        music['picUrl'] = r['album']['picUrl']
        music['url'] = ne.get_musics_url([r['id']])[0]['url']
        album['musics'].append(music)
    albums.append(album)

    album = { 'name': '我喜欢的音乐', 'musics': [] }
    for r in playlist_details[0]:
        music = {}
        music['name'] = r['name']
        music['picUrl'] = r['al']['picUrl']
        music['url'] = ne.get_musics_url([r['id']])[0]['url']
        album['musics'].append(music)
    albums.append(album)

    return render_template('index.html', title='Home', albums=albums)

if __name__ == "__main__":
    app.run(host='0.0.0.0', debug=True)
