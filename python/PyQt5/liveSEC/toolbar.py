# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore


class ToolBar(QtWidgets.QMainWindow):

    def __init__(self, parent, config):
        super().__init__()
        self.parent = parent
        self.config = config
        self.setMaximumHeight(config['maximum-height'])
        self.setStyleSheet('background-color:rgb(192, 192, 192, 255);')
