#!/usr/bin/python3
# -*- coding:utf-8

import sys
from PyQt5 import QtWidgets, QtGui, QtCore

from config import *
from navigation import Navigation
from toolbar import ToolBar


class MainWindow(QtWidgets.QMainWindow):

    def __init__(self):
        super().__init__()
        self.setWindowTitle(APP_NAME)
        self.setWindowIcon(QtGui.QIcon(APP_ICON))
        self.setMinimumSize(*WIN_SIZE)

        fg = self.frameGeometry()
        fg.moveCenter(QtWidgets.QDesktopWidget().availableGeometry().center())
        self.move(fg.topLeft())

        self.shortcut_ctrl_w = QtWidgets.QShortcut(QtGui.QKeySequence("CTRL+W"), self)
        self.shortcut_ctrl_w.activated.connect(QtCore.QCoreApplication.instance().quit)

        self.container = QtWidgets.QMainWindow()
        self.toolbar = ToolBar(self)
        self.navigation = Navigation(self)

        self.content_layout = QtWidgets.QHBoxLayout()
        self.content_layout.setSpacing(0)
        self.content_layout.addWidget(self.navigation)
        self.content_layout.addWidget(self.container)

        self.main_layout = QtWidgets.QVBoxLayout()
        self.main_layout.setSpacing(0)
        self.main_layout.setContentsMargins(0, 0, 0, 0)
        self.main_layout.addWidget(self.toolbar)
        self.main_layout.addLayout(self.content_layout)

        self.widget = QtWidgets.QWidget()
        self.widget.setLayout(self.main_layout)
        self.setCentralWidget(self.widget)

    def on_nav_item_pressed(self, item_id):
        if item_id == 'scene':
            self.container_scene = QtWidgets.QPushButton('scene')
            self.container.setCentralWidget(self.container_scene)
        else:
            self.container_music = QtWidgets.QPushButton('music')
            self.container.setCentralWidget(self.container_music)


if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    win = MainWindow()
    win.show()
    sys.exit(app.exec_())
