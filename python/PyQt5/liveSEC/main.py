#!/usr/bin/python3
# -*- coding:utf-8

import sys
from PyQt5 import QtWidgets, QtGui, QtCore

from config import *
from navigation import Navigation
from toolbar import ToolBar
from music import Music


class MainWindow(QtWidgets.QMainWindow):

    def __init__(self):
        super().__init__()
        self.setWindowTitle(APP_NAME)
        self.setWindowIcon(QtGui.QIcon(APP_ICON))
        self.setMinimumSize(*WIN_SIZE)

        ag = QtWidgets.QDesktopWidget().availableGeometry()
        h = ag.height()
        w = ag.width()
        if w*0.618 > h:
            h = int(h*3/4 + 0.5)
            w = int(h/0.618 + 0.5)
        else:
            w = int(w*3/4 + 0.5)
            h = int(w*0.618 + 0.5)
        self.resize(w, h)

        fg = self.frameGeometry()
        fg.moveCenter(ag.center())
        self.move(fg.topLeft())

        self.shortcut_ctrl_w = QtWidgets.QShortcut(QtGui.QKeySequence("CTRL+W"), self)
        self.shortcut_ctrl_w.activated.connect(QtCore.QCoreApplication.instance().quit)

        self.container = QtWidgets.QStackedWidget()
        self.container_scene = QtWidgets.QPushButton('scene')
        self.container_music = Music(self)
        self.container_weather = QtWidgets.QPushButton('weather')
        self.container.addWidget(self.container_scene)
        self.container.addWidget(self.container_music)
        self.container.addWidget(self.container_weather)

        self.toolbar = ToolBar(self)
        self.navigation = Navigation(self)

        self.layout1 = QtWidgets.QHBoxLayout()
        self.layout1.setSpacing(0)
        self.layout1.addWidget(self.navigation)
        self.layout1.addWidget(self.container)

        self.layout2 = QtWidgets.QVBoxLayout()
        self.layout2.setSpacing(0)
        self.layout2.setContentsMargins(0, 0, 0, 0)
        self.layout2.addWidget(self.toolbar)
        self.layout2.addLayout(self.layout1)

        self.widget = QtWidgets.QWidget()
        self.widget.setLayout(self.layout2)

        self.navigation_fold = QtWidgets.QPushButton(self.widget)
        self.navigation_fold.setText('<')
        self.navigation_fold.setFixedSize(12, 32)
        self.navigation_fold.setStyleSheet('''
            QPushButton {
                background-color: rgb(27, 29, 30, 112);
                color: #8e9a9a;
                border-radius: 2;
            }
            QPushButton:hover {
                color: #b6c6c6;
            }
            QPushButton:pressed {
                color: #dbefef;
            }
        ''')
        self.navigation_fold.move(0, 0)
        self.navigation_fold.clicked.connect(self.move_navigation_fold)
        self.move_navigation_fold()

        self.setCentralWidget(self.widget)

    def on_nav_item_pressed(self, item_id):
        if item_id == 'scene':
            self.container.setCurrentWidget(self.container_scene)
        elif item_id == 'music':
            self.container.setCurrentWidget(self.container_music)
        elif item_id == 'weather':
            self.container.setCurrentWidget(self.container_weather)

    def move_navigation_fold(self, resize=False):
        hfix = self.toolbar.height()
        hnav = self.navigation.height()
        wnav = self.navigation.width()

        top = int(hfix + hnav/2 - self.navigation_fold.height()/2)
        left = int(wnav - self.navigation_fold.width())

        if resize:
            self.navigation_fold.move(left if self.navigation_fold.frameGeometry().left() else 0, top)
            return

        if self.navigation_fold.frameGeometry().left():
            self.navigation_fold.move(0, top)
            self.navigation_fold.setText('>')
            self.navigation.hide()
        else:
            self.navigation.show()
            self.navigation_fold.setText('<')
            self.navigation_fold.move(left, top)

    def resizeEvent(self, event):
        self.move_navigation_fold(True)

if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    MainWindow().show()
    sys.exit(app.exec_())
