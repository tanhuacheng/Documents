# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore
from config import *


class ToolBar(QtWidgets.QMainWindow):

    def __init__(self, parent):
        super().__init__()
        self.parent = parent
        self.setMaximumHeight(TOOLBAR_MAX_HEIGHT)
        self.setStyleSheet('background-color:rgb(192, 192, 192, 255);')
