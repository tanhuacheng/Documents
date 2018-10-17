# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore
from .musics import Musics
from .mediaplayer import MediaPlayer


class QMusic(QtWidgets.QWidget):

    def __init__(self, config):
        super().__init__()
        self.config = config

        self.tree_song_list = self.TreeWidget(config['tree-song-list'])
        self.control = self.ControlBar(config['control-bar'])

        self.layout2 = QtWidgets.QVBoxLayout()
        self.layout2.setSpacing(0)
        self.layout2.setContentsMargins(0, 0, 0, 0)

        self.layout2.addWidget(self.tree_song_list)
        self.layout2.addWidget(self.control)

        self.setLayout(self.layout2)

        self.songs = Musics(config['musics']['cache-dir'])
        # self.songs.update() # this would take a while

        songs = list(self.songs.fetchall())
        for label in self.songs.get_labels():
            root = QtWidgets.QTreeWidgetItem(self.tree_song_list, [str(label)])
            root.playlist = list(filter(lambda x: label in x[-1], songs))
            for song in root.playlist:
                item = QtWidgets.QTreeWidgetItem(root, song[0:3])
                item.music = song

        root = QtWidgets.QTreeWidgetItem(self.tree_song_list, ['Others'])
        root.playlist = list(filter(lambda x: not x[-1], songs))
        for song in root.playlist:
            item = QtWidgets.QTreeWidgetItem(root, song[0:3])
            item.music = song

        self.media_player = self.QMediaPlayer()
        self.media_player.lengthChanged.connect(self.on_length_changed)
        self.media_player.positionChanged.connect(self.on_position_changed)
        self.media_player.endReached.connect(self.on_end_reached)
        self.media_player.volumeChanged.connect(self.control.set_volume)
        self.media_player.playlist = None
        self.media_player.current = None
        self.media_player.duration = 0

        self.tree_song_list.itemDoubleClicked.connect(self.on_playlist_double_clicked)

        self.control.button_prev.clicked.connect(self.on_button_prev_clicked)
        self.control.button_play.clicked.connect(self.on_button_play_clicked)
        self.control.button_pause.clicked.connect(self.on_button_pause_clicked)
        self.control.button_next.clicked.connect(self.on_button_next_clicked)

        self.control.progress_bar.valueChanged.connect(self.on_progress_bar_value_changed)

        self.control.callback_get_volume = self.media_player.volume
        self.control.volumeChanged.connect(self.media_player.set_volume)

    def on_playlist_double_clicked(self, item, column):
        parent = item.parent()
        if parent:
            if parent.playlist != self.media_player.playlist:
                self.media_player.playlist = parent.playlist
            if item.music != self.media_player.current:
                self.control.label_total_time.setText('00:00')
                self.control.label_played_time.setText('00:00')

                block = self.control.progress_bar.blockSignals(True)
                self.control.progress_bar.setValue(0)
                self.control.progress_bar.blockSignals(block)

                self.media_player.play(item.music[3])
                self.control.stack_play_pause.setCurrentWidget(self.control.button_pause)
                self.media_player.current = item.music
            elif self.control.stack_play_pause.currentWidget() == self.control.button_play:
                self.on_button_play_clicked()

    def on_button_prev_clicked(self):
        if self.media_player.playlist:
            if self.media_player.current:
                index = self.media_player.playlist.index(self.media_player.current) - 1
            else:
                index = 0
            index = (index + len(self.media_player.playlist)) % len(self.media_player.playlist)
            self.media_player.current = self.media_player.playlist[index]
            self.control.label_total_time.setText('00:00')
            self.control.label_played_time.setText('00:00')

            block = self.control.progress_bar.blockSignals(True)
            self.control.progress_bar.setValue(0)
            self.control.progress_bar.blockSignals(block)

            self.media_player.play(self.media_player.current[3])
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
            index = (index + len(self.media_player.playlist)) % len(self.media_player.playlist)
            self.media_player.current = self.media_player.playlist[index]
            self.control.label_total_time.setText('00:00')
            self.control.label_played_time.setText('00:00')

            block = self.control.progress_bar.blockSignals(True)
            self.control.progress_bar.setValue(0)
            self.control.progress_bar.blockSignals(block)

            self.media_player.play(self.media_player.current[3])
            self.control.stack_play_pause.setCurrentWidget(self.control.button_pause)

    def on_length_changed(self, length):
        duration = length / 1000
        self.media_player.duration = duration
        duration = int(duration + 0.5)
        self.control.label_total_time.setText('%02d:%02d' % (duration // 60, duration % 60))

    def on_position_changed(self, position):
        duration = round(self.media_player.duration * position / 100)
        self.control.label_played_time.setText('%02d:%02d' % (duration // 60, duration % 60))

        block = self.control.progress_bar.blockSignals(True)
        self.control.progress_bar.setValue(position)
        self.control.progress_bar.blockSignals(block)

    def on_end_reached(self):
        self.on_button_next_clicked()

    def on_progress_bar_value_changed(self, value):
        self.media_player.seek(value)

    class TreeWidget(QtWidgets.QTreeWidget):

        def __init__(self, config):
            super().__init__()
            self.config = config
            self.setHeaderHidden(True)

            column = len(config['column-width'])
            self.setColumnCount(column)
            for i in range(column):
                width = config['column-width'][i]
                if width > 0:
                    self.setColumnWidth(i, width)


    class ControlBar(QtWidgets.QWidget):

        volumeChanged = QtCore.pyqtSignal(int)

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
            self.label_total_time = QtWidgets.QLabel('00:00')
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

            self.button_volume_add_menu(self.button_volume_mute)
            self.button_volume_add_menu(self.button_volume_low)
            self.button_volume_add_menu(self.button_volume_medium)
            self.button_volume_add_menu(self.button_volume_high)
            self.callback_get_volume = None

            self.layout.addStretch(8)
            self.button_lyric = self.PushButton(config['button-lyric'])
            self.layout.addWidget(self.button_lyric)

            self.layout.addStretch(1)
            self.button_playlist = self.PushButton(config['button-playlist'])
            self.layout.addWidget(self.button_playlist)

            self.layout.addStretch(1)
            self.setLayout(self.layout)

        def button_volume_add_menu(self, button):
            button.setStyleSheet('''
                QPushButton::menu-indicator {
                    image: None;
                }
            ''' + button.styleSheet())

            button.widget = QtWidgets.QWidget()
            button.widget.slider = QtWidgets.QSlider()
            button.widget.slider.setMinimum(0)
            button.widget.slider.setMaximum(100)
            button.widget.layout = QtWidgets.QGridLayout()
            button.widget.layout.addWidget(button.widget.slider, 0, 0, 1, 1)
            button.widget.setLayout(button.widget.layout)

            button.action = QtWidgets.QWidgetAction(button)
            button.action.setDefaultWidget(button.widget)

            button.menu = QtWidgets.QMenu()
            button.menu.addAction(button.action)
            button.setMenu(button.menu)

            button.menu.aboutToShow.connect(self.volume_menu_about_to_show)
            button.widget.slider.valueChanged.connect(self.volume_changed)
            button.menu.aboutToHide.connect(self.volume_menu_about_to_hide)

        def volume_menu_about_to_show(self):
            self.stack_volume.current_widget = self.stack_volume.currentWidget()
            if self.callback_get_volume:
                self.set_volume(self.callback_get_volume())

        def volume_changed(self, value):
            if self.stack_volume.current_widget != self.button_volume_mute:
                block = self.button_volume_mute.widget.slider.blockSignals(True)
                self.button_volume_mute.widget.slider.setValue(value)
                self.button_volume_mute.widget.slider.blockSignals(block)
            if self.stack_volume.current_widget != self.button_volume_low:
                block = self.button_volume_low.widget.slider.blockSignals(True)
                self.button_volume_low.widget.slider.setValue(value)
                self.button_volume_low.widget.slider.blockSignals(block)
            if self.stack_volume.current_widget != self.button_volume_medium:
                block = self.button_volume_medium.widget.slider.blockSignals(True)
                self.button_volume_medium.widget.slider.setValue(value)
                self.button_volume_medium.widget.slider.blockSignals(block)
            if self.stack_volume.current_widget != self.button_volume_high:
                block = self.button_volume_high.widget.slider.blockSignals(True)
                self.button_volume_high.widget.slider.setValue(value)
                self.button_volume_high.widget.slider.blockSignals(block)

            self.set_volume_button(value)
            self.volumeChanged.emit(value)

        def volume_menu_about_to_hide(self):
            self.stack_volume.current_widget = self.stack_volume.currentWidget()

        def set_volume(self, value):
            block = self.button_volume_mute.widget.slider.blockSignals(True)
            self.button_volume_mute.widget.slider.setValue(value)
            self.button_volume_mute.widget.slider.blockSignals(block)

            block = self.button_volume_low.widget.slider.blockSignals(True)
            self.button_volume_low.widget.slider.setValue(value)
            self.button_volume_low.widget.slider.blockSignals(block)

            block = self.button_volume_medium.widget.slider.blockSignals(True)
            self.button_volume_medium.widget.slider.setValue(value)
            self.button_volume_medium.widget.slider.blockSignals(block)

            block = self.button_volume_high.widget.slider.blockSignals(True)
            self.button_volume_high.widget.slider.setValue(value)
            self.button_volume_high.widget.slider.blockSignals(block)

            self.set_volume_button(value)

        def set_volume_button(self, value):
            if value == 0:
                self.stack_volume.setCurrentWidget(self.button_volume_mute)
            elif value < 34:
                self.stack_volume.setCurrentWidget(self.button_volume_low)
            elif value < 67:
                self.stack_volume.setCurrentWidget(self.button_volume_medium)
            else:
                self.stack_volume.setCurrentWidget(self.button_volume_high)


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


    class QMediaPlayer(QtCore.QObject, MediaPlayer):

        lengthChanged = QtCore.pyqtSignal(int)
        positionChanged = QtCore.pyqtSignal(int)
        endReached = QtCore.pyqtSignal()
        volumeChanged = QtCore.pyqtSignal(int)

        def __init__(self):
            super().__init__()

        def on_length_changed(self, value):
            self.lengthChanged.emit(value)

        def on_position_changed(self, value):
            self.positionChanged.emit(value)

        def on_end_reached(self):
            self.endReached.emit()

        def on_audio_volume(self, value):
            self.volumeChanged.emit(value)
