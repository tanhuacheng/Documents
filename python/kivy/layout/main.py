#!/usr/bin/python3
# -*- coding:utf-8

'layout experiment'

__author__ = 'tanhc'


import kivy
kivy.require('1.10.0')

from kivy.app import App
from kivy.uix.label import Label
from kivy.uix.button import Button
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.pagelayout import PageLayout


class PageSwitcherButton(Button):

    def __init__(self, text, binder, **kwargs):
        super(PageSwitcherButton, self).__init__(**kwargs)
        self.text = text
        self.bind(on_press=binder)


def switch_to_x_page(ins, x):
    ins.parent.pagewindow.page = x
    for child in ins.parent.children:
        child.background_color = (1, 1, 1, 1)
    ins.background_color = (1, 0, 0, 1)


class PageSwitcher(BoxLayout):

    def __init__(self, pagewindow, **kwargs):
        super(PageSwitcher, self).__init__(**kwargs)
        self.pagewindow = pagewindow
        self.orientation = 'horizontal'

        b1 = PageSwitcherButton('page1', lambda x: switch_to_x_page(x, 0))
        b2 = PageSwitcherButton('page2', lambda x: switch_to_x_page(x, 1))
        b2.background_color = (1, 0, 0, 1)
        b3 = PageSwitcherButton('page3', lambda x: switch_to_x_page(x, 2))

        self.add_widget(b1)
        self.add_widget(b2)
        self.add_widget(b3)


class PageWindow(PageLayout):

    def __init__(self, **kwargs):
        super(PageWindow, self).__init__(**kwargs)
        self.border = 40
        self.swipe_threshold = 0.1
        self.page = 1
        self.add_widget(Button(text='label1'))
        self.add_widget(Button(text='label2'))
        self.add_widget(Button(text='label3'))


class MainBox(BoxLayout):

    def __init__(self, **kwargs):
        super(MainBox, self).__init__(**kwargs)
        self.orientation = 'vertical'

        pw = PageWindow()
        ps = PageSwitcher(pw)
        ps.size_hint = (1, 0.1)

        self.add_widget(ps)
        self.add_widget(pw)


class MyApp(App):

    def build(self):
        return MainBox()


def main():
    MyApp().run()


if __name__ == '__main__':
    main()
