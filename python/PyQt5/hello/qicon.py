#!/usr/bin/python3
# -*- coding:utf-8

import sys
import os
from PyQt5.QtWidgets import QApplication, QWidget
from PyQt5.QtGui import QIcon

class Example(QWidget):

    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):
        self.setGeometry(300, 300, 300, 220)
        self.setWindowTitle('Icon')
        self.setWindowIcon(QIcon(os.path.join(os.path.curdir, 'web.jpg')))
        self.show()


app = QApplication(sys.argv)
exm = Example()
sys.exit(app.exec_())
