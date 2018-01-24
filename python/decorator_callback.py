#!/usr/bin/python3
# -*- coding:utf-8

class Test():

    def feature(self):
        self.callback()

    def decorate(self, func):
        self.callback = func
        return func

test = Test()

@test.decorate
def foo():
    print('test')

test.feature()
