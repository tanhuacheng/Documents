# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore
from config import *


class Music(QtWidgets.QWidget):

    def __init__(self, parent):
        super().__init__()
        self.parent = parent

        self.playlist = QtWidgets.QTreeWidget()
        self.lyric = QtWidgets.QTextEdit()

        self.layout1 = QtWidgets.QHBoxLayout()
        self.layout1.setSpacing(0)
        self.layout1.addWidget(self.playlist)
        self.layout1.addWidget(self.lyric)

        self.control = ControlBar(CONFIG_MUSIC['control-bar'])

        self.layout2 = QtWidgets.QVBoxLayout()
        self.layout2.setSpacing(0)
        self.layout2.setContentsMargins(0, 0, 0, 0)
        self.layout2.addLayout(self.layout1)
        self.layout2.addWidget(self.control)

        self.setLayout(self.layout2)


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
        self.button_prev = PushButton(config['button-prev'])
        self.layout.addWidget(self.button_prev)

        self.layout.addStretch(1)
        self.stack_play_pause = QtWidgets.QStackedWidget()
        self.stack_play_pause.setFixedSize(*config['stack-play-and-pause']['size'])
        self.button_play = PushButton(config['stack-play-and-pause']['button-play'])
        self.button_pause = PushButton(config['stack-play-and-pause']['button-pause'])
        self.stack_play_pause.addWidget(self.button_play)
        self.stack_play_pause.addWidget(self.button_pause)
        self.layout.addWidget(self.stack_play_pause)

        self.layout.addStretch(1)
        self.button_next = PushButton(config['button-next'])
        self.layout.addWidget(self.button_next)

        self.layout.addStretch(2)
        self.stack_order = QtWidgets.QStackedWidget()
        self.stack_order.setFixedSize(*config['stack-order']['size'])
        self.button_loop = PushButton(config['stack-order']['button-loop'])
        self.button_repeat = PushButton(config['stack-order']['button-repeat'])
        self.button_random = PushButton(config['stack-order']['button-random'])
        self.stack_order.addWidget(self.button_loop)
        self.stack_order.addWidget(self.button_repeat)
        self.stack_order.addWidget(self.button_random)
        self.layout.addWidget(self.stack_order)

        self.layout.addStretch(8)
        self.label_played_time = QtWidgets.QLabel('00:00')
        self.label_played_time.setStyleSheet(config['label-played-time']['style-sheet'])
        self.layout.addWidget(self.label_played_time)

        self.layout.addStretch(1)
        self.progress_bar = ProgressBar(config['progress-bar'])
        self.layout.addWidget(self.progress_bar, 48)

        self.layout.addStretch(1)
        self.label_total_time = QtWidgets.QLabel('03:56')
        self.label_total_time.setStyleSheet(config['label-total-time']['style-sheet'])
        self.layout.addWidget(self.label_total_time)

        self.layout.addStretch(2)
        self.stack_volume = QtWidgets.QStackedWidget()
        self.stack_volume.setFixedSize(*config['stack-volume']['size'])
        self.button_volume_mute = PushButton(config['stack-volume']['button-volume-mute'])
        self.button_volume_low = PushButton(config['stack-volume']['button-volume-low'])
        self.button_volume_medium = PushButton(config['stack-volume']['button-volume-medium'])
        self.button_volume_high = PushButton(config['stack-volume']['button-volume-high'])
        self.stack_volume.addWidget(self.button_volume_mute)
        self.stack_volume.addWidget(self.button_volume_low)
        self.stack_volume.addWidget(self.button_volume_medium)
        self.stack_volume.addWidget(self.button_volume_high)
        self.layout.addWidget(self.stack_volume)

        self.layout.addStretch(8)
        self.button_lyric = PushButton(config['button-lyric'])
        self.layout.addWidget(self.button_lyric)

        self.layout.addStretch(1)
        self.button_playlist = PushButton(config['button-playlist'])
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
        self.setStyleSheet('''
            QSlider::groove:horizontal {
                height: 8px;
                border-radius: 2px;
            }
            QSlider::sub-page:horizontal {
                background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                    stop: 0 #c0d900, stop: 1 #3f4600);
                border-radius: 2px;
            }
            QSlider::add-page:horizontal {
                background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                    stop: 0 #c7d9d9, stop: 1 #404646);
                border-radius: 2px;
            }
            QSlider::handle:horizontal {
                background-color: #c0d900;
                width: 8px;
                height: 14px;
                margin-top: -3px;
                margin-bottom: -3px;
                border-radius: 3px;
            }
            QSlider::handle:horizontal:hover {
                background-color: #c7d9d9;
            }
            QSlider::handle:horizontal:pressed {
                background-color: #dcefef;
            }
        ''')
        self.setStyle(self.ProxyStyle(self.style()))


    class ProxyStyle(QtWidgets.QProxyStyle):

        def __init__(self, style=None):
            super().__init__(style)

        def styleHint(self, hint, *args, **kwargs):
            if hint == QtWidgets.QStyle.SH_Slider_AbsoluteSetButtons:
                return QtCore.Qt.LeftButton | QtCore.Qt.MidButton
            return super().styleHint(hint, *args, **kwargs)
