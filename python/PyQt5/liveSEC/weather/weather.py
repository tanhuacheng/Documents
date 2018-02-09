# -*- coding:utf-8

from PyQt5 import QtWidgets, QtCore
from .get_location import get_location
from .get_weather import get_weather


class Weather(QtWidgets.QWidget):

    def __init__(self, config):
        super().__init__()
        self.config = config

        self.stack_weather = QtWidgets.QStackedWidget()
        self.cities = []

        self.layout = QtWidgets.QVBoxLayout()
        self.layout.addWidget(self.stack_weather)
        self.setLayout(self.layout)

        city = self.City(get_location())
        self.stack_weather.addWidget(city)
        self.cities.append(city)

    class City(QtWidgets.QWidget):

        def __init__(self, city):
            super().__init__()
            self.city = city

            self.text_edit = QtWidgets.QTextEdit()
            self.text_edit.setReadOnly(True)

            self.layout = QtWidgets.QGridLayout()
            self.layout.setContentsMargins(0, 0, 0, 0)
            self.layout.addWidget(self.text_edit, 0, 0, 1, 1)
            self.setLayout(self.layout)

            self.update()

        def update(self):
            weather = get_weather(self.city)

            body = ''
            for w in weather['forecast']:
                body += '''
                    <tr>
                        <td>%s</td>
                        <td>%s</td>
                        <td>%s</td>
                        <td>%s</td>
                        <td>%s</td>
                    </tr>
                ''' % (w['date'], w['high'], w['low'], w['day']['type'], w['night']['type'])

            zhishu = ''
            for w in weather['zhishus']:
                zhishu += '''
                    <h3>%s: %s</h3>
                    <p>%s</p>
                ''' % (w['name'], w['value'], w['detail'])

            text = '''
                <h1>%s</h1><br>
                <p>温度: %s</p>
                <p>湿度: %s</p>
                <p>风力: %s</p>
                <p>日出: %s</p>
                <p>日落: %s</p>
                <hr>
                <table width="80%%" align="center">%s</table>
                <hr>
                %s
            ''' % (weather['city'],
                   weather['wendu'], weather['shidu'], weather['fengli'], weather['sunrise_1'],
                   weather['sunset_1'], body, zhishu)
            self.text_edit.setText(text)

            self.weather = weather
