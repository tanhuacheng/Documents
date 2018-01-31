# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore


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

        self.control = Control()

        self.layout2 = QtWidgets.QVBoxLayout()
        self.layout2.setSpacing(0)
        self.layout2.setContentsMargins(0, 0, 0, 0)
        self.layout2.addLayout(self.layout1)
        self.layout2.addWidget(self.control)

        self.setLayout(self.layout2)


class Control(QtWidgets.QWidget):

    def __init__(self):
        super().__init__()

        self.setFixedHeight(60)
        self.pal = QtGui.QPalette(self.palette())
        self.pal.setColor(QtGui.QPalette.Background, QtGui.QColor(int('002B36', 16)))
        self.setAutoFillBackground(True)
        self.setPalette(self.pal)

        self.media_prev = PlayButton(32, (
            './icones/control_media_prev_3.png',
            './icones/control_media_prev_2.png',
            './icones/control_media_prev_1.png',
            ))

        self.media_play = PlayButton(32, (
            './icones/control_media_play_3.png',
            './icones/control_media_play_2.png',
            './icones/control_media_play_1.png',
            ))
        self.media_pause = PlayButton(32, (
            './icones/control_media_pause_3.png',
            './icones/control_media_pause_2.png',
            './icones/control_media_pause_1.png',
            ))

        self.media_next = PlayButton(32, (
            './icones/control_media_next_3.png',
            './icones/control_media_next_2.png',
            './icones/control_media_next_1.png',
            ))

        self.media_order_loop = PlayButton(24, (
            './icones/control_media_order_loop_3.png',
            './icones/control_media_order_loop_2.png',
            './icones/control_media_order_loop_1.png',
            ))
        self.media_order_repeat = PlayButton(24, (
            './icones/control_media_order_repeat_3.png',
            './icones/control_media_order_repeat_2.png',
            './icones/control_media_order_repeat_1.png',
            ))
        self.media_order_random = PlayButton(24, (
            './icones/control_media_order_random_3.png',
            './icones/control_media_order_random_2.png',
            './icones/control_media_order_random_1.png',
            ))

        self.media_play_pause = QtWidgets.QStackedWidget()
        self.media_play_pause.setFixedSize(32, 32)
        self.media_play_pause.addWidget(self.media_play)
        self.media_play_pause.addWidget(self.media_pause)

        self.media_order_stack = QtWidgets.QStackedWidget()
        self.media_order_stack.setFixedSize(24, 24)
        self.media_order_stack.addWidget(self.media_order_random)
        self.media_order_stack.addWidget(self.media_order_loop)
        self.media_order_stack.addWidget(self.media_order_repeat)

        self.layout = QtWidgets.QHBoxLayout()
        self.layout.addWidget(self.media_prev)
        self.layout.addWidget(self.media_play_pause)
        self.layout.addWidget(self.media_next)
        self.layout.addWidget(self.media_order_stack)
        self.layout.insertSpacing(3, 16)
        self.layout.addStretch()

        self.play_time = QtWidgets.QLabel('00:00')
        self.play_time.setStyleSheet('color: #93A1A1; margin: 0px 0px 3px 0px;')

        self.play_progress = QtWidgets.QSlider(QtCore.Qt.Horizontal)
        self.play_progress.setMinimumWidth(180)
        self.play_progress.setTickPosition(QtWidgets.QSlider.TicksBothSides)
        self.play_progress.setStyleSheet('''
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

        self.total_time = QtWidgets.QLabel('03:56')
        self.total_time.setStyleSheet('color: #93A1A1; margin: 0px 0px 3px 0px;')

        self.layout.addWidget(self.play_time)
        self.layout.insertSpacing(7, 4)
        self.layout.addWidget(self.play_progress)
        self.layout.insertSpacing(9, 4)
        self.layout.addWidget(self.total_time)

        self.volume = PlayButton(32, (
            './icones/control_playlist_3.png',
            './icones/control_playlist_2.png',
            './icones/control_playlist_1.png',
        ))
        self.layout.addWidget(self.volume)

        self.layout.addStretch()

        self.playlist = PlayButton(32, (
            './icones/control_playlist_3.png',
            './icones/control_playlist_2.png',
            './icones/control_playlist_1.png',
        ))
        self.layout.addWidget(self.playlist)


        self.setLayout(self.layout)


class PlayButton(QtWidgets.QPushButton):

    def __init__(self, size, icones):
        super().__init__()

        self.setFixedSize(size, size)
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
        ''' % icones)
