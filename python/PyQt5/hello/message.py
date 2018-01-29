#!/usr/bin/python3
# -*- coding:utf-8

import sys
from PyQt5.QtWidgets import QApplication, QWidget, QMessageBox

class Example(QWidget):

    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):
        self.setGeometry(300, 300, 250, 150)
        self.setWindowTitle('Quit button')
        self.show()

    def closeEvent(self, event):
        reply = QMessageBox.question(self, 'Message', 'Are you sure to quit?',
                    QMessageBox.Yes | QMessageBox.No, QMessageBox.Yes)
        if reply == QMessageBox.Yes:
            event.accept()
        else:
            event.ignore()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    exm = Example()
    sys.exit(app.exec_())
