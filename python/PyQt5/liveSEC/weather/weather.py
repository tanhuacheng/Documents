# -*- coding:utf-8

from PyQt5 import QtWidgets, QtCore


class QAddCities(QtWidgets.QWidget):

    def __init__(self):
        super().__init__()

        # city input
        self.input = QtWidgets.QLineEdit()
        self.input.setPlaceholderText('city')

        self.input_hlayout = QtWidgets.QHBoxLayout()
        self.input_hlayout.addStretch(1)
        self.input_hlayout.addWidget(self.input, 2)
        self.input_hlayout.addStretch(1)

        self.input_vlayout = QtWidgets.QVBoxLayout()
        self.input_vlayout.addStretch()
        self.input_vlayout.addLayout(self.input_hlayout)

        # main layout
        self.main_layout = QtWidgets.QVBoxLayout()
        self.main_layout.addLayout(self.input_vlayout, 382)
        self.main_layout.addStretch(618)

        self.setLayout(self.main_layout)


class QWeather(QtWidgets.QTabWidget):

    def __init__(self, config):
        super().__init__()

        self.config = config

        self.home = QAddCities()
        self.addTab(self.home, '+')
