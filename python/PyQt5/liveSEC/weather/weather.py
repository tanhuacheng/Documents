# -*- coding:utf-8

from PyQt5 import QtWidgets, QtCore
import json

from .accuweather import AccuWeather


class QAddCities(QtWidgets.QWidget):

    def __init__(self, config, api_weather):
        super().__init__()

        self.config = config
        self.api_weather = api_weather

        # input layout
        self.input = QtWidgets.QLineEdit()
        self.input.setPlaceholderText('search cities here')
        self.input.setFocusPolicy(QtCore.Qt.StrongFocus)

        self.input_hlayout = QtWidgets.QHBoxLayout()
        self.input_hlayout.addStretch(1)
        self.input_hlayout.addWidget(self.input, 1)
        self.input_hlayout.addStretch(1)

        # main layout
        self.main_layout = QtWidgets.QVBoxLayout()
        self.main_layout.addStretch(382)
        self.main_layout.addLayout(self.input_hlayout)
        self.main_layout.addStretch(618)

        self.setLayout(self.main_layout)

        # input completer
        self.input_completer = QtWidgets.QCompleter()
        self.input_completer_model = QtCore.QStringListModel()
        self.input_completer.setModel(self.input_completer_model)
        self.input.setCompleter(self.input_completer)

        # signals
        self.input.textEdited.connect(self.input_text_edited)
        self.input.returnPressed.connect(self.input_return_pressed)
        self.input_completer.activated[QtCore.QModelIndex].connect(self.input_completer_activated)
        self.input_completer.highlighted.connect(self.highlighted)

        # variables
        try:
            with open(self.config['history'], 'r') as fp:
                self.history = json.load(fp)
        except:
            self.history = {}

        self.key = ''

    def highlighted(self, text):
        print(text)

    def showEvent(self, event):
        self.input.setFocus()

    def input_text_edited(self, text):
        self.input.setFocus()

    def input_return_pressed(self):
        text = self.input.text()
        print(text)

        text = text.strip()
        if not text:
            return

        if text in self.history:
            self.key = text
            self.input_completer_model.setStringList(map(lambda x: x[0], self.history[text]))
            return

        print('search ...')
        cities = self.api_weather.locations_search(text)
        if cities:
            def _convert(city):
                return ','.join((city['LocalizedName'],
                                 city['Country']['LocalizedName'],
                                 city['AdministrativeArea']['LocalizedName'])), city['Key']

            self.history[text] = tuple(map(_convert, cities))
            with open(self.config['history'], 'w') as fp:
                json.dump(self.history, fp)

            self.key = text
            self.input_completer_model.setStringList(map(lambda x: x[0], self.history[text]))

    def input_completer_activated(self, index):
        # TODO handle result, selected index is index.row()
        print('get code', self.history[self.key][index.row()][1])


class QWeather(QtWidgets.QTabWidget):

    def __init__(self, config):
        super().__init__()

        self.config = config
        self.api_weather = AccuWeather()

        self.home = QAddCities(self.config['locations'], self.api_weather)
        self.addTab(self.home, '+')
