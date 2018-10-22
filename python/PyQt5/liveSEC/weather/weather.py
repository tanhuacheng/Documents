# -*- coding:utf-8

import re
import json

from PyQt5 import QtWidgets, QtCore, QtGui

from .accuweather import AccuWeather


class QLineEdit(QtWidgets.QLineEdit):

    upPressed = QtCore.pyqtSignal()
    downPressed = QtCore.pyqtSignal()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.installEventFilter(self)

    def eventFilter(self, obj, event):
        if obj == self and event.type() == QtCore.QEvent.KeyPress:
            if event.key() == QtCore.Qt.Key_Up:
                self.upPressed.emit()
                return True
            if event.key() == QtCore.Qt.Key_Down:
                self.downPressed.emit()
                return True

        return super().eventFilter(obj, event)


class QTreeWidget(QtWidgets.QTreeWidget):

    escPressed = QtCore.pyqtSignal()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.installEventFilter(self)

    def eventFilter(self, obj, event):
        if obj == self and event.type() == QtCore.QEvent.KeyPress:
            if event.key() == QtCore.Qt.Key_Escape:
                self.escPressed.emit()
                return True

        return super().eventFilter(obj, event)


class QLocations(QtWidgets.QWidget):

    commit = QtCore.pyqtSignal(list)

    def __init__(self, config, weatherapi):
        super().__init__()

        self.config = config
        self.weatherapi = weatherapi

        # input layout
        self.input = QLineEdit()
        self.input.setPlaceholderText('搜索城市')
        self.input.setValidator(QtGui.QRegExpValidator(QtCore.QRegExp('\w*')))

        self.input_hlayout = QtWidgets.QHBoxLayout()
        self.input_hlayout.addStretch(1)
        self.input_hlayout.addWidget(self.input, 1)
        self.input_hlayout.addStretch(1)

        # tree layout
        self.tree = QTreeWidget()
        self.tree.setRootIsDecorated(False)
        self.tree.setColumnCount(3)
        self.tree.setHeaderLabels(['地区', '国家', '行政区'])
        self.tree.hide()

        self.tree_hlayout = QtWidgets.QHBoxLayout()
        self.tree_hlayout.addStretch(1)
        self.tree_hlayout.addWidget(self.tree, 1)
        self.tree_hlayout.addStretch(1)

        self.tree_vlayout = QtWidgets.QVBoxLayout()
        self.tree_vlayout.addLayout(self.tree_hlayout)
        self.tree_vlayout.addStretch()

        # main layout
        self.main_layout = QtWidgets.QVBoxLayout()
        self.main_layout.setSpacing(0)
        self.main_layout.addStretch(382)
        self.main_layout.addLayout(self.input_hlayout)
        self.main_layout.addLayout(self.tree_vlayout, 618)

        self.setLayout(self.main_layout)

        # variables
        try:
            with open(self.config['locations'], 'r') as fp:
                self.locations = json.load(fp)
        except:
            self.locations = []

        try:
            with open(self.config['searches'], 'r') as fp:
                self.searches = set(json.load(fp))
        except:
            self.searches = set()

        # signals
        self.input.textEdited.connect(self.input_text_edited)
        self.input.returnPressed.connect(self.input_return_pressed)
        self.input.upPressed.connect(self.input_up_pressed)
        self.input.downPressed.connect(self.input_down_pressed)

        self.tree.itemActivated.connect(self.tree_item_activated)
        self.tree.escPressed.connect(self.tree_esc_pressed)

    def show_tree(self, locations, text):
        locations_matched = [x for x in locations if re.search('.*'.join(text), ''.join(x[:-1]))]
        if locations_matched:
            self.tree.addTopLevelItems([QtWidgets.QTreeWidgetItem(x) for x in locations_matched])
            self.tree.show()
            return True

        return False

    def search_locations(self, text):
        locations_search = self.weatherapi.search_locations(text)
        locations = []
        for location in locations_search:
            if [x for x in self.locations if x[-1] == location['Key']]:
                continue
            locations.append((location['LocalizedName'],
                              location['Country']['LocalizedName'],
                              location['AdministrativeArea']['LocalizedName'],
                              location['Key']))

        if locations:
            self.locations += locations
            with open(self.config['locations'], 'w') as fp:
                json.dump(self.locations, fp)
            self.show_tree(locations, text)

    def update_searches(self, text):
        if text in self.searches:
            return False

        self.searches.add(text)
        with open(self.config['searches'], 'w') as fp:
            json.dump(list(self.searches), fp)

        return True

    def showEvent(self, event):
        self.input.setFocus()

    def input_text_edited(self, text):
        self.tree.clear()
        self.tree.hide()

        if not text:
            return
        if self.show_tree(self.locations, text):
            return
        if not self.update_searches(text):
            return

        self.search_locations(text)

    def input_return_pressed(self):
        self.tree.clear()
        self.tree.hide()

        text = self.input.text()
        if not text:
            return

        self.update_searches(text)
        self.search_locations(text)

    def input_up_down_pressed(self, selector):
        if self.tree.topLevelItemCount():
            self.tree.setFocus()
            if not self.tree.selectedItems():
                item = self.tree.currentItem()
            else:
                item = selector(self.tree.currentItem())
                if not item:
                    item = self.tree.currentItem()
            self.tree.setCurrentItem(item)

    def input_up_pressed(self):
        self.input_up_down_pressed(self.tree.itemAbove)

    def input_down_pressed(self):
        self.input_up_down_pressed(self.tree.itemBelow)

    def tree_item_activated(self, item, column):
        if item:
            self.commit.emit([item.text(i) for i in range(item.columnCount())])
            self.input.clear()
            self.input.setFocus()
            self.tree.clear()
            self.tree.hide()

    def tree_esc_pressed(self):
        self.input.setFocus()


class QCity(QtWidgets.QWidget):

    def __init__(self, config, weatherapi, city):
        super().__init__()

        self.config = config
        self.weatherapi = weatherapi
        self.city = city

        # TODO should work in a thread
        #  self.current_conditions = self.weatherapi.current_conditions(self.city['location'][-1])


class QWeather(QtWidgets.QTabWidget):

    def __init__(self, config):
        super().__init__()

        self.config = config
        self.weatherapi = AccuWeather()

        self.home = QLocations(self.config['locations'], self.weatherapi)
        self.addTab(self.home, '+')

        try:
            with open(self.config['settings'], 'r') as fp:
                self.settings = json.load(fp)
        except:
            self.settings = {'current': 0, 'cities': []}

        current = self.settings['current']
        for city in self.settings['cities']:
            self.add_city(city)
        self.settings['current'] = current
        self.setCurrentIndex(current)

        self.home.commit.connect(self.commit_location)
        self.currentChanged.connect(self.current_changed)
        self.tabBarDoubleClicked.connect(self.tab_bar_double_clicked)

    def add_city(self, city):
        tab = QCity(self.config['city'], self.weatherapi, city)
        self.settings['current'] = self.count() - 1
        self.insertTab(self.settings['current'], tab, city['location'][0])
        self.setCurrentIndex(self.settings['current'])

    def update_settings(self):
        with open(self.config['settings'], 'w') as fp:
            json.dump(self.settings, fp)

    def commit_location(self, location):
        for i, city in enumerate(self.settings['cities']):
            if location[-1] == city['location'][-1]:
                self.settings['current'] = i
                self.setCurrentIndex(i)
                break
        else:
            city = {'location': location}
            self.settings['cities'].append(city)
            self.add_city(city)

        self.update_settings()

    def current_changed(self, index):
        if self.settings['current'] != index:
            self.settings['current'] = index
            self.update_settings()

    def tab_bar_double_clicked(self, index):
        if index == self.count() - 1:
            return

        self.removeTab(index)
        del self.settings['cities'][index]
        self.update_settings()
