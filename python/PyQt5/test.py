#!/usr/bin/python3

#  import sys
#  from PyQt5.QtWidgets import QMainWindow, QApplication, QPushButton, QWidget, QAction, QTabWidget,QVBoxLayout
#  from PyQt5.QtGui import QIcon
#  from PyQt5.QtCore import pyqtSlot
#
#  class App(QMainWindow):
#
#      def __init__(self):
#          super().__init__()
#          self.title = 'PyQt5 tabs - pythonspot.com'
#          self.left = 0
#          self.top = 0
#          self.width = 300
#          self.height = 200
#          self.setWindowTitle(self.title)
#          self.setGeometry(self.left, self.top, self.width, self.height)
#
#          self.table_widget = MyTableWidget(self)
#          self.setCentralWidget(self.table_widget)
#
#          self.show()
#
#  class MyTableWidget(QWidget):
#
#      def __init__(self, parent):
#          super(QWidget, self).__init__(parent)
#          self.layout = QVBoxLayout(self)
#
#          # Initialize tab screen
#          self.tabs = QTabWidget()
#          self.tab1 = QWidget()
#          self.tab2 = QWidget()
#          self.tabs.resize(300,200)
#
#          # Add tabs
#          self.tabs.addTab(self.tab1,"Tab 1")
#          self.tabs.addTab(self.tab2,"Tab 2")
#
#          # Create first tab
#          self.tab1.layout = QVBoxLayout(self)
#          self.pushButton1 = QPushButton("PyQt5 button")
#          self.tab1.layout.addWidget(self.pushButton1)
#          self.tab1.setLayout(self.tab1.layout)
#
#          # Add tabs to widget
#          self.layout.addWidget(self.tabs)
#          self.setLayout(self.layout)
#
#      @pyqtSlot()
#      def on_click(self):
#          print("\n")
#          for currentQTableWidgetItem in self.tableWidget.selectedItems():
#              print(currentQTableWidgetItem.row(), currentQTableWidgetItem.column(), currentQTableWidgetItem.text())
#
#  if __name__ == '__main__':
#      app = QApplication(sys.argv)
#      ex = App()
#      sys.exit(app.exec_())
#

"""
ZetCode PyQt5 tutorial

In this example, we position two push
buttons in the bottom-right corner
of the window.

Author: Jan Bodnar
Website: zetcode.com
Last edited: August 2017
"""

import sys
from PyQt5.QtWidgets import (QWidget, QPushButton,
    QHBoxLayout, QVBoxLayout, QApplication)


class Example(QWidget):

    def __init__(self):
        super().__init__()

        self.initUI()


    def initUI(self):

        okButton = QPushButton("OK")
        cancelButton = QPushButton("Cancel")

        hbox = QHBoxLayout()
        hbox.addStretch(1)
        hbox.addWidget(okButton)
        hbox.addWidget(cancelButton)

        vbox = QVBoxLayout()
        vbox.addStretch(1)
        vbox.addLayout(hbox)

        self.setLayout(vbox)

        self.setGeometry(300, 300, 300, 150)
        self.setWindowTitle('Buttons')
        self.show()


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = Example()
    sys.exit(app.exec_())
