# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore
import vlc
from .netease import NetEase


class Music(QtWidgets.QWidget):

    def __init__(self, config):
        super().__init__()
        self.config = config

        self.playlist = QtWidgets.QTreeWidget()
        self.playlist.setHeaderHidden(True)
        self.playlist.setColumnWidth(0, 200)
        self.playlist.setColumnCount(2)

        self.lyric = QtWidgets.QTextEdit()

        self.layout1 = QtWidgets.QHBoxLayout()
        self.layout1.setSpacing(0)
        self.layout1.addWidget(self.playlist)
        self.layout1.addWidget(self.lyric)

        self.control = self.ControlBar(config['control-bar'])

        self.layout2 = QtWidgets.QVBoxLayout()
        self.layout2.setSpacing(0)
        self.layout2.setContentsMargins(0, 0, 0, 0)
        self.layout2.addLayout(self.layout1)
        self.layout2.addWidget(self.control)

        self.setLayout(self.layout2)

        self.netease = NetEase(config['netease-config-dir'])
        if not self.netease.is_login():
            self.netease.login('13418629507', '*')
        recommends = self.netease.get_recommend()
        self.netease.playlists = [{'name': '每日推荐', 'detail': recommends}]
        for l in self.netease.get_playlists():
            l['detail'] = self.netease.get_playlist_detail(l['id'], l['trackCount'])
            self.netease.playlists.append(l)

        for l in self.netease.playlists:
            root = QtWidgets.QTreeWidgetItem(self.playlist, [l['name']])
            root.playlist = l['detail']
            for music in root.playlist:
                artists = ''
                ar = music['artists'] if 'artists' in music else music['ar']
                for artist in ar:
                    artists += artist['name']
                    artists += '/'
                if len(artists):
                    artists = artists[0:-1]
                item = QtWidgets.QTreeWidgetItem(root, [music['name'], artists])
                item.music = music

        self.media_player = self.MediaPlayer()
        self.media_player.event_playing = self.on_playing
        self.media_player.event_position_changed = self.on_position_changed
        self.media_player.playlist = None
        self.media_player.current = None

        self.playlist.itemDoubleClicked.connect(self.on_playlist_double_clicked)

        self.control.button_prev.clicked.connect(self.on_button_prev_clicked)
        self.control.button_play.clicked.connect(self.on_button_play_clicked)
        self.control.button_pause.clicked.connect(self.on_button_pause_clicked)
        self.control.button_next.clicked.connect(self.on_button_next_clicked)

    def on_playlist_double_clicked(self, item, column):
        parent = item.parent()
        if parent:
            if parent.playlist != self.media_player.playlist:
                self.media_player.playlist = parent.playlist
            if item.music != self.media_player.current:
                url = self.netease.get_musics_url([item.music['id']])
                print(url)
                mrl = url[0]['url']
                self.media_player.play(mrl)
                self.control.stack_play_pause.setCurrentWidget(self.control.button_pause)
                self.media_player.current = item.music

    def on_button_prev_clicked(self):
        if self.media_player.playlist:
            if self.media_player.current:
                index = self.media_player.playlist.index(self.media_player.current) - 1
            else:
                index = 0
            self.media_player.current = self.media_player.playlist[index]
            mrl = self.netease.get_musics_url([self.media_player.current['id']])[0]['url']
            self.media_player.play(mrl)
            self.control.stack_play_pause.setCurrentWidget(self.control.button_pause)

    def on_button_play_clicked(self):
        if self.media_player.current:
            self.media_player.play()
            self.control.stack_play_pause.setCurrentWidget(self.control.button_pause)

    def on_button_pause_clicked(self):
        if self.media_player.current:
            self.media_player.play()
            self.control.stack_play_pause.setCurrentWidget(self.control.button_play)

    def on_button_next_clicked(self):
        if self.media_player.playlist:
            if self.media_player.current:
                index = self.media_player.playlist.index(self.media_player.current) + 1
            else:
                index = 0
            index = index % len(self.media_player.playlist)
            self.media_player.current = self.media_player.playlist[index]
            mrl = self.netease.get_musics_url([self.media_player.current['id']])[0]['url']
            self.media_player.play(mrl)
            self.control.stack_play_pause.setCurrentWidget(self.control.button_pause)

    def on_playing(self):
        duration = int(self.media_player.media_duration() / 1000)
        self.media_player.duration = duration
        self.control.label_total_time.setText('%02d:%02d' % (duration // 60, duration % 60))

    def on_position_changed(self):
        position = self.media_player.player_position()
        duration = int(self.media_player.duration * position)
        self.control.label_played_time.setText('%02d:%02d' % (duration // 60, duration % 60))
        self.control.progress_bar.setValue(100*position)


    class ControlBar(QtWidgets.QWidget):

        def __init__(self, config):
            super().__init__()
            self.config = config
            self.setMinimumHeight(config['minimum-height'])
            self.setMaximumHeight(config['maximum-height'])
            self.pal = QtGui.QPalette(self.palette())
            self.pal.setColor(QtGui.QPalette.Background, QtGui.QColor(config['background-color']))
            self.setPalette(self.pal)
            self.setAutoFillBackground(True)

            self.layout = QtWidgets.QHBoxLayout()

            self.layout.addStretch(1)
            self.button_prev = self.PushButton(config['button-prev'])
            self.layout.addWidget(self.button_prev)

            self.layout.addStretch(1)
            self.stack_play_pause = QtWidgets.QStackedWidget()
            self.stack_play_pause.setFixedSize(*config['stack-play-and-pause']['size'])
            self.button_play = self.PushButton(config['stack-play-and-pause']['button-play'])
            self.button_pause = self.PushButton(config['stack-play-and-pause']['button-pause'])
            self.stack_play_pause.addWidget(self.button_play)
            self.stack_play_pause.addWidget(self.button_pause)
            self.layout.addWidget(self.stack_play_pause)

            self.layout.addStretch(1)
            self.button_next = self.PushButton(config['button-next'])
            self.layout.addWidget(self.button_next)

            self.layout.addStretch(2)
            self.stack_order = QtWidgets.QStackedWidget()
            self.stack_order.setFixedSize(*config['stack-order']['size'])
            self.button_loop = self.PushButton(config['stack-order']['button-loop'])
            self.button_repeat = self.PushButton(config['stack-order']['button-repeat'])
            self.button_random = self.PushButton(config['stack-order']['button-random'])
            self.stack_order.addWidget(self.button_loop)
            self.stack_order.addWidget(self.button_repeat)
            self.stack_order.addWidget(self.button_random)
            self.layout.addWidget(self.stack_order)

            self.layout.addStretch(8)
            self.label_played_time = QtWidgets.QLabel('00:00')
            self.label_played_time.setStyleSheet(config['label-played-time']['style-sheet'])
            self.layout.addWidget(self.label_played_time)

            self.layout.addStretch(1)
            self.progress_bar = self.ProgressBar(config['progress-bar'])
            self.layout.addWidget(self.progress_bar, 48)

            self.layout.addStretch(1)
            self.label_total_time = QtWidgets.QLabel('03:56')
            self.label_total_time.setStyleSheet(config['label-total-time']['style-sheet'])
            self.layout.addWidget(self.label_total_time)

            self.layout.addStretch(2)
            self.stack_volume = QtWidgets.QStackedWidget()
            self.stack_volume.setFixedSize(*config['stack-volume']['size'])
            self.button_volume_mute = self.PushButton(config['stack-volume']['button-volume-mute'])
            self.button_volume_low = self.PushButton(config['stack-volume']['button-volume-low'])
            self.button_volume_medium = \
                self.PushButton(config['stack-volume']['button-volume-medium'])
            self.button_volume_high = self.PushButton(config['stack-volume']['button-volume-high'])
            self.stack_volume.addWidget(self.button_volume_mute)
            self.stack_volume.addWidget(self.button_volume_low)
            self.stack_volume.addWidget(self.button_volume_medium)
            self.stack_volume.addWidget(self.button_volume_high)
            self.layout.addWidget(self.stack_volume)

            self.layout.addStretch(8)
            self.button_lyric = self.PushButton(config['button-lyric'])
            self.layout.addWidget(self.button_lyric)

            self.layout.addStretch(1)
            self.button_playlist = self.PushButton(config['button-playlist'])
            self.layout.addWidget(self.button_playlist)

            self.layout.addStretch(1)
            self.setLayout(self.layout)


        class PushButton(QtWidgets.QPushButton):

            def __init__(self, config):
                super().__init__()
                self.config = config
                self.setFixedSize(*config['size'])
                self.setStyleSheet('''
                    QPushButton {
                        border:0px;
                        border-image: url(%s);
                    }
                    QPushButton:hover {
                        border-image: url(%s);
                    }
                    QPushButton:pressed {
                        border-image: url(%s)
                    }
                ''' % config['border-images'])


        class ProgressBar(QtWidgets.QSlider):

            def __init__(self, config):
                super().__init__(QtCore.Qt.Horizontal)
                self.config = config
                self.setStyleSheet(config['style-sheet'])
                self.setStyle(self.ProxyStyle(self.style()))


            class ProxyStyle(QtWidgets.QProxyStyle):

                def __init__(self, style=None):
                    super().__init__(style)

                def styleHint(self, hint, *args, **kwargs):
                    if hint == QtWidgets.QStyle.SH_Slider_AbsoluteSetButtons:
                        return QtCore.Qt.LeftButton | QtCore.Qt.MidButton
                    return super().styleHint(hint, *args, **kwargs)


    class MediaPlayer(object):

        def __init__(self):
            self.instance = vlc.Instance()
            self.player = vlc.MediaPlayer(self.instance)
            self.player.event_manager().event_attach(
                vlc.EventType.MediaPlayerPlaying, self.on_playing)
            self.player.event_manager().event_attach(
                vlc.EventType.MediaPlayerPositionChanged, self.on_position_changed)
            self.media = None

            self.event_playing = None
            self.event_position_changed = None

        def play(self, mrl=None):
            if mrl:
                if self.media:
                    self.player.stop()
                    self.media.release()
                self.media = self.instance.media_new(mrl)
                self.player.set_media(self.media)
                self.player.play()
            else:
                self.player.pause() # toggle pause

        def is_playing(self):
            return self.player.is_playing()

        def on_playing(self, *args, **kwargs):
            if self.event_playing:
                self.event_playing()

        def media_duration(self):
            return self.media.get_duration()

        def on_position_changed(self, *args, **kwargs):
            if self.event_position_changed:
                self.event_position_changed()

        def player_position(self):
            return self.player.get_position()

#  {
#      'artists': [{'briefDesc': '', 'musicSize': 0, 'id': 747030, 'picId': 0, 'picUrl': 'http://p1.music.126.net/6y-UleORITEDbvrOLV0Q8A==/5639395138885805.jpg', 'trans': '', 'name': 'Vicetone', 'alias': [], 'albumSize': 0, 'img1v1Id': 0, 'img1v1Url': 'http://p1.music.126.net/6y-UleORITEDbvrOLV0Q8A==/5639395138885805.jpg'},
#          {'briefDesc': '', 'musicSize': 0, 'id': 1097153, 'picId': 0, 'picUrl': 'http://p1.music.126.net/6y-UleORITEDbvrOLV0Q8A==/5639395138885805.jpg', 'trans': '', 'name': 'Cozi Zuehlsdorff', 'alias': [], 'albumSize': 0, 'img1v1Id': 0, 'img1v1Url': 'http://p1.music.126.net/6y-UleORITEDbvrOLV0Q8A==/5639395138885805.jpg'}],
#      'name': 'Nevada',
#      'album': {'picId_str': '18517974835045498', 'company': 'Monstercat', 'subType': '混音版', 'blurPicUrl': 'http://p1.music.126.net/8uFCXr2mUoXNAK1EJgVBhw==/18517974835045498.jpg', 'artist': {'briefDesc': '', 'musicSize': 0, 'id': 0, 'picId': 0, 'picUrl': 'http://p1.music.126.net/6y-UleORITEDbvrOLV0Q8A==/5639395138885805.jpg', 'trans': '', 'name': '', 'alias': [], 'albumSize': 0, 'img1v1Id': 0, 'img1v1Url': 'http://p1.music.126.net/6y-UleORITEDbvrOLV0Q8A==/5639395138885805.jpg'}, 'briefDesc': '', 'artists': [{'briefDesc': '', 'musicSize': 0, 'id': 747030, 'picId': 0, 'picUrl': 'http://p1.music.126.net/6y-UleORITEDbvrOLV0Q8A==/5639395138885805.jpg', 'trans': '', 'name': 'Vicetone', 'alias': [], 'albumSize': 0, 'img1v1Id': 0, 'img1v1Url': 'http://p1.music.126.net/6y-UleORITEDbvrOLV0Q8A==/5639395138885805.jpg'}], 'pic': 18517974835045498, 'picId': 18517974835045498, 'description': '', 'name': 'Nevada', 'size': 1, 'picUrl': 'http://p1.music.126.net/8uFCXr2mUoXNAK1EJgVBhw==/18517974835045498.jpg', 'type': 'EP/Single', 'companyId': 0, 'commentThreadId': 'R_AL_3_34750015', 'copyrightId': 0, 'publishTime': 1466697600007, 'tags': '', 'alias': [], 'status': 0, 'id': 34750015, 'songs': []},
#      'duration': 208561
#  }