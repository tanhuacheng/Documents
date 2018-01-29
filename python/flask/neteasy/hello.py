#!/usr/bin/python3
# -*- coding:utf-8

import os, sys
from flask import Flask, render_template, redirect, url_for
import net_ease
from getpass import getpass

app = Flask(__name__)
ne = net_ease.NetEase(os.path.curdir)

if not ne.is_login():
    phone = input('phone number: ')
    password = getpass('[%s] password for %s: ' % (sys.argv[0], phone))
    ne.login(phone, password)

recommends = ne.get_recommend()
for i in range(0, len(recommends)):
    try:
        recommends[i]['url'] = ne.get_musics_url([recommends[i]['id']])[0]['url']
    except:
        pass

playlist_details = []
playlists = ne.get_playlists()
for l in playlists:
    detail = ne.get_playlist_detail(l['id'], l['trackCount'])
    for i in range(0, len(detail)):
        try:
            detail[i]['url'] = ne.get_musics_url([detail[i]['id']])[0]['url']
        except:
            pass
    playlist_details.append(detail)

@app.route('/')
@app.route('/index')
def index():
    global recommends
    global playlists
    global playlist_details

    albums = []

    album = { 'name': '每日推荐', 'musics': [] }
    for r in recommends:
        if r['url']:
            music = {}
            music['name'] = r['name']
            music['picUrl'] = r['album']['picUrl']
            music['url'] = r['url']
            album['musics'].append(music)
    albums.append(album)

    try:
        for i in range(len(playlists)):
            album = { 'name': playlists[i]['name'], 'musics': [] }
            for r in playlist_details[i]:
                if r['url']:
                    music = {}
                    music['name'] = r['name']
                    music['picUrl'] = r['al']['picUrl']
                    music['url'] = r['url']
                    album['musics'].append(music)
            albums.append(album)
    except:
        pass

    return render_template('index.html', title='Home', albums=albums)

@app.route('/refresh')
def refresh():
    global recommends
    global playlist_details
    global playlists

    recommends = ne.get_recommend()
    for i in range(0, len(recommends)):
        try:
            recommends[i]['url'] = ne.get_musics_url([recommends[i]['id']])[0]['url']
        except:
            pass

    playlist_details = []
    playlists = ne.get_playlists()
    for l in playlists:
        detail = ne.get_playlist_detail(l['id'], l['trackCount'])
        for i in range(0, len(detail)):
            try:
                detail[i]['url'] = ne.get_musics_url([detail[i]['id']])[0]['url']
            except:
                pass
        playlist_details.append(detail)

    return redirect(url_for('index'))

if __name__ == "__main__":
    app.run(host='0.0.0.0', debug=True)
