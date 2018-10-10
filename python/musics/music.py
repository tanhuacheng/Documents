import os
import sqlite3
import json
import requests
import re
from functools import reduce

import douban


class Music:

    def __init__(self, dir_):
        def _mkdir(dir_):
            try:
                os.mkdir(dir_)
            except FileExistsError:
                pass
            except:
                raise

        self._dir = dir_
        self._dir_music = os.path.join(dir_, 'music')
        self._dir_lyric = os.path.join(dir_, 'lyric')
        self._dir_picture = os.path.join(dir_, 'picture')

        _mkdir(self._dir)
        _mkdir(self._dir_music)
        _mkdir(self._dir_lyric)
        _mkdir(self._dir_picture)

        self._db_conn = sqlite3.connect(os.path.join(dir_, 'library.db'))
        self._db_cursor = self._db_conn.cursor()

        self._db_cursor.execute('''
            CREATE TABLE IF NOT EXISTS music (
                title           TEXT PRIMARY KEY NOT NULL,
                artist          TEXT,
                play_length     TEXT,
                music           TEXT NOT NULL,
                lyrics          TEXT,
                picture         TEXT,
                labels          TEXT
            );
        ''')
        self._db_conn.commit()

        labels = map(lambda x: set(json.loads(x[0])) if x[0] else set(),
                     self._db_cursor.execute('SELECT labels FROM music'))
        self._labels = reduce(lambda x, y: x | y, labels, set())

        self._fetcher = ({'domain': 'douban', 'fetcher': douban.fetch_songs},)

    def update(self):
        for fetcher in self._fetcher:
            songs = fetcher['fetcher']()
            if not songs:
                continue

            for song in songs:
                url = song.get('url')
                if not url:
                    continue

                title = song.get('title')
                if not title:
                    continue

                dbtitle = self._db_cursor.execute('SELECT title FROM music WHERE title=?', (title,))
                if dbtitle.fetchone():
                    continue

                file_music = re.search(r'\w+\.\w+$', url)
                file_music = (fetcher['domain'] + '_' + file_music.group()) if file_music else None
                if not file_music:
                    continue

                file_music_full = os.path.join(self._dir_music, file_music)
                if not os.access(file_music_full, os.F_OK):
                    try:
                        req = requests.get(url, timeout=32)
                        with open(file_music_full, 'wb') as f:
                            f.write(req.content)
                    except:
                        continue

                lyrics = song.get('lyrics')
                if lyrics:
                    file_lyrics = re.search(r'^\w+', file_music).group() + '.lyc'
                    file_lyrics_full = os.path.join(self._dir_lyric, file_lyrics)
                    if not os.access(file_lyrics_full, os.F_OK):
                        with open(file_lyrics_full, 'wb') as f:
                            f.write(lyrics.encode())
                else:
                    file_lyrics = None

                picture = song.get('picture')
                if picture:
                    picture = picture[0]
                    file_picture = re.search(r'\w+\.\w+$', picture)
                    file_picture = file_picture.group() if file_picture else None
                    file_picture = fetcher['domain'] + '_' + file_picture if file_picture else None
                    if file_picture:
                        file_picture_full = os.path.join(self._dir_picture, file_picture)
                        if not os.access(file_picture_full, os.F_OK):
                            try:
                                req = requests.get(picture, timeout=32)
                                with open(file_picture_full, 'wb') as f:
                                    f.write(req.content)
                            except:
                                file_picture = None
                else:
                    file_picture = None

                artist = song.get('artist_name')
                play_length = song.get('play_length')
                labels = json.dumps([])

                self._db_cursor.execute('INSERT INTO music VALUES (?,?,?,?,?,?,?)',
                    (title, artist, play_length, file_music, file_lyrics, file_picture, labels))
                self._db_conn.commit()

    def _convert(self, x):
        return x[0:3] + \
               (os.path.join(self._dir_music, x[3]) if x[3] else None,) + \
               (os.path.join(self._dir_lyric, x[4]) if x[4] else None,) + \
               (os.path.join(self._dir_picture, x[5]) if x[5] else None,) + \
               (json.loads(x[6]),)

    def fetchall(self):
        return map(self._convert, self._db_cursor.execute('SELECT * FROM music'))

    def remove(self, title):
        song = self._db_cursor.execute('SELECT * FROM music WHERE title=?', (title,)).fetchone()
        if song:
            song = self._convert(song)
            for f in song[3:6]:
                if f:
                    try:
                        os.remove(f)
                    except:
                        pass
            self._db_cursor.execute('DELETE FROM music WHERE title=?', (title,))
            self._db_conn.commit()

    def set_labels(self, title, labels):
        self._db_cursor.execute('UPDATE music SET labels=? WHERE title=?',
                                (json.dumps(list(set(labels))), title))
        self._db_conn.commit()

        if self._db_conn.total_changes > 0:
            self._labels = self._labels | set(labels)
