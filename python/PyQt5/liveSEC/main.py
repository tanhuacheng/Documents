#!/usr/bin/python3
# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore
from navigation import Navigation
from music.music import Music


class MainWindow(QtWidgets.QMainWindow):

    def __init__(self, config):
        super().__init__()
        self.config = config
        self.setWindowTitle(config['app-name'])
        self.setWindowIcon(QtGui.QIcon(config['app-icon']))
        self.setMinimumSize(*config['minimum-size'])

        ag = QtWidgets.QDesktopWidget().availableGeometry()
        w, h = map(lambda x: round(x*3/4), (ag.width(), ag.height()))
        if w*0.618 > h:
            w = round(h/0.618)
        else:
            h = round(w*0.618)

        self.resize(w, h)
        fg = self.frameGeometry()
        fg.moveCenter(ag.center())
        self.move(fg.topLeft())

        self.shortcut_quit = QtWidgets.QShortcut(QtGui.QKeySequence(config['shortcut-quit']), self)
        self.shortcut_quit.activated.connect(QtCore.QCoreApplication.instance().quit)

        self.hlayout = QtWidgets.QHBoxLayout()
        self.hlayout.setSpacing(0)
        self.hlayout.setContentsMargins(0, 0, 0, 0)

        self.navigation = Navigation(config['navigation'])
        self.navigation.set_current_item('music')
        self.hlayout.addWidget(self.navigation, config['h-stretch-navigation'])

        self.container = QtWidgets.QStackedWidget()
        self.container_music = Music(config['container']['music'])
        self.container.addWidget(self.container_music)
        self.container.setCurrentWidget(self.container_music)

        self.navigation.callback_current_item_changed = self.on_navigation_current_item_changed

        self.hlayout.addWidget(self.container, config['h-stretch-container'])

        self.widget = QtWidgets.QWidget()
        self.widget.setLayout(self.hlayout)

        self.navigation_folder = self.NavigationFolder(self.widget, config['navigation-folder'])
        self.navigation_folder.setText('>')
        self.navigation_folder.folded = True
        self.navigation_folder.clicked.connect(self.on_navigation_folder_clicked)

        self.navigation.hide()

        self.setCentralWidget(self.widget)

    def on_navigation_current_item_changed(self, obj, item_id):
        if item_id == 'music':
            self.container.setCurrentWidget(self.container_music)

    def on_navigation_folder_clicked(self):
        if self.navigation_folder.folded:
            self.navigation.show()
            self.navigation_folder.setText('<')
        else:
            self.navigation.hide()
            self.navigation_folder.setText('>')
        self.navigation_folder.folded = not self.navigation_folder.folded
        self.set_navigation_folder_geometry()

    def set_navigation_folder_geometry(self):
        if self.navigation_folder.folded:
            winh, winw = self.container.height(), self.container.width()
        else:
            winh, winw = self.navigation.height(), self.navigation.width()

        h, w = map(lambda x: x / self.config['navigation-folder']['nav-sizefactor'], (winh, winw))
        h = max(h, self.config['navigation-folder']['minimum-height'])
        if h/4 > w:
            h = w*4
        else:
            w = h/4
        h, w = map(int, (h, w))

        self.navigation_folder.resize(w, h)

        top = int(winh/2 - h/2)
        left = int((winw - w) if not self.navigation_folder.folded else 0)
        self.navigation_folder.move(left, top)

    def showEvent(self, event):
        self.set_navigation_folder_geometry()

    def resizeEvent(self, event):
        self.set_navigation_folder_geometry()


    class NavigationFolder(QtWidgets.QPushButton):

        def __init__(self, parent, config):
            super().__init__(parent)
            self.config = config
            self.setStyleSheet(config['style-sheet'])


if __name__ == '__main__':
    import sys
    import config

    app = QtWidgets.QApplication(sys.argv)
    win = MainWindow(config.main_config)
    win.show()
    sys.exit(app.exec_())
