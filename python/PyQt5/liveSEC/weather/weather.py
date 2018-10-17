# -*- coding:utf-8

from PyQt5 import QtWidgets, QtCore
from .accuweather import AccuWeather


class QWeather(QtWidgets.QWidget):

    def __init__(self, config):
        super().__init__()
        self.config = config

        self.main_layout = QtWidgets.QVBoxLayout()
        self.main_layout.setSpacing(0)
        self.main_layout.setContentsMargins(0, 0, 0, 0)

        self.layout_add_city = QtWidgets.QHBoxLayout()
        self.label_add_city = QtWidgets.QLabel('Add City')
        self.edit_add_city = QtWidgets.QLineEdit()
        self.label_add_city.setBuddy(self.edit_add_city)
        self.layout_add_city.addWidget(self.label_add_city)
        self.layout_add_city.addWidget(self.edit_add_city)

        self.model = QtCore.QStringListModel(['abc'])
        self.completer_add_city = QtWidgets.QCompleter()
        self.completer_add_city.setModel(self.model)
        self.edit_add_city.setCompleter(self.completer_add_city)

        self.main_layout.addLayout(self.layout_add_city)

        self.setLayout(self.main_layout)

        self.edit_add_city.editingFinished.connect(self.edit_add_city_text_finished)

        self.weather = AccuWeather()

    def edit_add_city_text_finished(self):
        text = self.edit_add_city.text()
        print(text)
        if text:
            citys = self.weather.locations_search(text)
            if citys and isinstance(citys, list):
                stringlist = []
                for city in citys:
                    stringlist.append(city['LocalizedName'] + ',' +
                                      city['Country']['LocalizedName'] + ',' +
                                      city['AdministrativeArea']['LocalizedName'])
                if stringlist:
                    self.model.setStringList(stringlist)
