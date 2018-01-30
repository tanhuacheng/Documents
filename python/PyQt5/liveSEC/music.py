# -*- coding:utf-8

from PyQt5 import QtWidgets


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

        self.control = QtWidgets.QPushButton()
        self.control.setMinimumHeight(48)

        self.layout2 = QtWidgets.QVBoxLayout()
        self.layout2.setSpacing(0)
        self.layout2.setContentsMargins(0, 0, 0, 0)
        self.layout2.addLayout(self.layout1)
        self.layout2.addWidget(self.control)

        self.setLayout(self.layout2)
