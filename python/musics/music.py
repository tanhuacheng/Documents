import os
import sqlite3
import json
import requests
import re
from functools import reduce

import douban


class Music:

    def __init__(self, dir_):
        def _mkdir(path):
            try:
                os.mkdir(path)
            except FileExistsError:
                pass
            except:
                raise

        self._dir = dir_
        self._dir_music = os.path.join(dir_, 'music')
        self._dir_lyric = os.path.join(dir_, 'lyric')
        self._dir_picture = os.path.join(dir_, 'picture')

        try:
            _mkdir(self._dir)
            _mkdir(self._dir_music)
            _mkdir(self._dir_lyric)
            _mkdir(self._dir_picture)
        except:
            raise

        self._db_conn = sqlite3.connect(os.path.join(dir_, 'library.db'))
        self._db_cursor = self._db_conn.cursor()

        self._db_cursor.execute('''
            CREATE TABLE IF NOT EXISTS music (
                title           TEXT PRIMARY KEY NOT NULL,
                artist_name     TEXT,
                play_length     TEXT,
                file_music      TEXT,
                file_lyric      TEXT,
                file_picture    TEXT,
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
                # title must be present
                title = song.get('title')
                if not title:
                    continue

                artist_name = song.get('artist_name')
                play_length = song.get('play_length')

                try:
                    artist_name = song['artist_name']
                    play_length = song['play_length']

                    url = song['url']
                    picture = song['picture']

                    file_name = title + re.search(r'\.\w+$', url).group()

                    if os.access(file_name, os.F_OK):
                        continue

                    req = requests.get(url, timeout=32)
                    with open(os.path.join(self._data_dir, file_name), 'wb') as f:
                        f.write(req.content)

                    self._db.update((title, artist_name, play_length, file_name, 0))
                except:
                    continue

    #  def list(self):
    #      return self._db.list()

    #  def remove(self, title):
    #      file_name = self._db.remove(title)
    #      if file_name:
    #          os.remove(os.path.join(self._data_dir, file_name))

    #  def set_favorite(self, title, favorite):
    #      self._db.set_favorite(title, favorite)


    #  class DB:
    #
    #      def __init__(self, data_dir):
    #
    #      def list(self):
    #          return list(self._cursor.execute('SELECT * FROM musics'))
    #
    #      def update(self, data):
    #          self._cursor.execute('INSERT OR REPLACE INTO musics VALUES (?,?,?,?,?)', data)
    #          self._conn.commit()
    #
    #      def remove(self, title):
    #          file_name = self._cursor.execute(
    #                  'SELECT file_name FROM musics WHERE title=?', (title,)).fetchone()
    #          if file_name:
    #              self._cursor.execute('DELETE FROM musics WHERE title=?', (title,))
    #              self._conn.commit()
    #
    #          return file_name[0] if file_name else None
    #
    #      def set_favorite(self, title, favorite):
    #          self._cursor.execute('UPDATE musics SET favorite=? WHERE title=?', (favorite, title))
    #          self._conn.commit()



{
    'title': 'Sally Baby (塞利 宝贝)',
    'artist_name': '梁晓雪',
    'play_length': '4:11',

    'url': 'http://mr3.doubanio.com/faa2bfd2a3c8822804ac56fb93435019/0/fm/song/p2861155_128k.mp3',
    'lyrics': '词/曲/唱：梁晓雪\n\n',
    'picture': ['https://img1.doubanio.com/view/site/large/public/3a2c39f61bde3db.jpg'],

    'publish_date': '2018-09-29',
    'styles': ['Bluse蓝调布鲁斯', 'City-Pop城市流行'],

    'rec_url': 'https://site.douban.com/101191/?s=734879',
    'subject_id': '0',
    'sid': '734879',
    'is_downloadable': False,
    'is_collected': False,
    'play_count': '1871',
    'is_selling': True,
    'is_commentable': True,
    'label': '',

    'artist': {
        'name': '梁晓雪',
        'cover_color': ['#c4aaff', '#2c2933', ''],
        'url': 'https://site.douban.com/kulu/',
        'style': '民谣 Folk',
        'song_count': 184,
        'genre_url': 'https://music.douban.com/artists/genre_page/4/',
        'id': '101191',
        'is_royalty_artist': True,
        'follower': 70284,
        'picture': 'https://img1.doubanio.com/view/site/large/public/3a2c39f61bde3db.jpg',
        'kind': 'artist'
    },
}
