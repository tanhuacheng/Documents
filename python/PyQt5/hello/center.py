#!/usr/bin/python3
# -*- coding:utf-8

import sys
from PyQt5.QtWidgets import QApplication, QWidget, QDesktopWidget

class Example(QWidget):

    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):
        self.resize(250, 250)
        self.center()

        self.setWindowTitle('Quit button')
        self.show()

    def center(self):
        qr = self.frameGeometry()
        cp = QDesktopWidget().availableGeometry().center()
        qr.moveCenter(cp)
        self.move(qr.topLeft())


if __name__ == '__main__':
    app = QApplication(sys.argv)
    exm = Example()
    sys.exit(app.exec_())
