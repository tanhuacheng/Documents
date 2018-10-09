import requests
import re


_URL_CHART = 'https://music.douban.com/chart'
_URL_PLAYLIST = 'https://music.douban.com/j/artist/playlist'


def fetch_songs():
    try:
        req = requests.get(_URL_CHART, timeout=32)
        if not req or req.status_code != requests.codes.ok or not req.text:
            return []
    except:
        return []

    playlist = re.search(r'<ul class="col5">(?:.|\n)+?</ul>', req.text)
    playlist = playlist.group() if playlist else None
    if not playlist:
        return []

    sids = []
    while True:
        sid = re.search(r'data-sid="(\d+)"', playlist)
        if sid:
            playlist = playlist[sid.end():]
            sid = sid.groups()

        if sid:
            sids.append(sid[0])
        else:
            break

    if not sids:
        return []

    try:
        req = requests.post(_URL_PLAYLIST, data={'sids': ','.join(sids)}, timeout=32)
        songs = req.json()
        if 'r' in songs and songs['r'] == 0 and 'songs' in songs:
            return songs['songs']
    except:
        pass

    return []
