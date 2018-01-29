#!/usr/bin/python3
# -*- coding:utf-8


import sys
from PyQt5 import QtWidgets, QtGui, QtCore, Qt
from config import *


class MainWindow(QtWidgets.QMainWindow):

    def __init__(self):
        super().__init__()
        self.setWindowTitle(APP_NAME)
        self.setWindowIcon(QtGui.QIcon(APP_ICON))

        self.show()


if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    win = MainWindow()
    sys.exit(app.exec_())
