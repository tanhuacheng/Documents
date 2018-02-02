# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui


class ToolBar(QtWidgets.QWidget):

    def __init__(self, config):
        super().__init__()
        self.config = config
        self.setMinimumHeight(config['minimum-height'])
        self.pal = QtGui.QPalette(self.palette())
        self.pal.setColor(QtGui.QPalette.Background, QtGui.QColor(config['background-color']))
        self.setPalette(self.pal)
        self.setAutoFillBackground(True)
