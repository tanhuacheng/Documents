#!/usr/bin/python3
# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore
from toolbar import ToolBar
from navigation import Navigation
from music import Music


class MainWindow(QtWidgets.QMainWindow):

    def __init__(self, config):
        super().__init__()
        self.config = config
        self.setWindowTitle(config['app-name'])
        self.setWindowIcon(QtGui.QIcon(config['app-icon']))
        self.setMinimumSize(*config['minimum-size'])

        ag = QtWidgets.QDesktopWidget().availableGeometry()
        w, h = map(lambda x: int(x*3/4), (ag.width(), ag.height()))
        if w*0.618 > h:
            w = int(h/0.618 + 0.5)
        else:
            h = int(w*0.618 + 0.5)
        self.resize(w, h)
        fg = self.frameGeometry()
        fg.moveCenter(ag.center())
        self.move(fg.topLeft())

        self.shortcut_quit = QtWidgets.QShortcut(QtGui.QKeySequence(config['shortcut-quit']), self)
        self.shortcut_quit.activated.connect(QtCore.QCoreApplication.instance().quit)

        self.vlayout = QtWidgets.QVBoxLayout()
        self.vlayout.setSpacing(0)
        self.vlayout.setContentsMargins(0, 0, 0, 0)

        self.toolbar = ToolBar(config['toolbar'])
        self.vlayout.addWidget(self.toolbar, config['v-stretch-toolbar'])

        self.hlayout = QtWidgets.QHBoxLayout()
        self.hlayout.setSpacing(0)

        self.navigation = Navigation(config['navigation'])
        self.navigation.item_pressed_event = self.on_navigation_item_pressed
        self.navigation.set_current_item('music')
        self.hlayout.addWidget(self.navigation, config['h-stretch-navigation'])

        self.container = QtWidgets.QStackedWidget()
        self.container_scene = QtWidgets.QPushButton('scene')           # TODO
        self.container_music = Music(config['container']['music'])
        self.container_weather = QtWidgets.QPushButton('weather')       # TODO
        self.container.addWidget(self.container_scene)
        self.container.addWidget(self.container_music)
        self.container.addWidget(self.container_weather)
        self.container.setCurrentWidget(self.container_music)

        self.hlayout.addWidget(self.container, config['h-stretch-container'])
        self.vlayout.addLayout(self.hlayout, config['v-stretch-container'])

        self.widget = QtWidgets.QWidget()
        self.widget.setLayout(self.vlayout)

        self.navigation_folder = self.NavigationFolder(self.widget, config['navigation-folder'])
        self.navigation_folder.setText('<')
        self.navigation_folder.move(1, 0)
        self.navigation_folder.clicked.connect(self.move_navigation_folder)

        self.setCentralWidget(self.widget)

    def on_navigation_item_pressed(self, obj, item_id):
        if item_id == 'scene':
            self.container.setCurrentWidget(self.container_scene)
        elif item_id == 'music':
            self.container.setCurrentWidget(self.container_music)
        elif item_id == 'weather':
            self.container.setCurrentWidget(self.container_weather)

    def move_navigation_folder(self, resize=False):
        hide = self.navigation_folder.frameGeometry().left() < 1
        if not resize:
            if hide:
                self.navigation.show()
            else:
                self.navigation.hide()

        hfix = self.toolbar.height()
        hnav = self.container.height()
        wnav = self.navigation.width()

        hb, wb = map(lambda x: x / self.config['navigation-folder']['nav-sizefactor'], (hnav, wnav))
        hb = max(hb, self.config['navigation-folder']['minimum-height'])
        if hb/4 > wb:
            hb = wb*4
        else:
            wb = hb/4
        hb, wb = map(int, (hb, wb))
        self.navigation_folder.resize(wb, hb)

        top = int(hfix + hnav/2 - hb/2)

        if resize:
            if hide:
                self.navigation_folder.move(0, top)
            else:
                self.navigation_folder.move(int(wnav - wb), top)
            return

        if hide:
            self.navigation_folder.setText('<')
            self.navigation_folder.move(int(wnav - wb), top)
        else:
            self.navigation_folder.setText('>')
            self.navigation_folder.move(0, top)

    def showEvent(self, event):
        self.move_navigation_folder(True)

    def resizeEvent(self, event):
        self.move_navigation_folder(True)


    class NavigationFolder(QtWidgets.QPushButton):

        def __init__(self, parent, config):
            super().__init__(parent)
            self.config = config
            self.setStyleSheet(config['style-sheet'])


if __name__ == '__main__':
    import sys
    import config

    app = QtWidgets.QApplication(sys.argv)
    MainWindow(config.main_config).show()
    sys.exit(app.exec_())
