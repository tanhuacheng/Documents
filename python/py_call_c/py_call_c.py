#!/usr/bin/python3
# -*- coding:utf-8

import os
from ctypes import *

lib = cdll.LoadLibrary(os.path.join(os.path.dirname(__file__), 'libtest.so'))

# Note: Make sure you keep references to CFUNCTYPE() objects as long as they are used from C code.
# ctypes doesn't, and if you don't, they may be garbage collected, crashing your program when a
# callback is made.
#
# Also, note that if the callback function is called in a thread created outside of Python's control
# (e.g. by the foreign code that calls the callback), ctypes creates a new dummy Python thread on
# every invocation. This behavior is correct for most purposes, but it means that values stored with
# threading.local will not survive across different callbacks, even when those calls are made from
# the same C thread.

def foo(bar):
    print('py: foo', bar.x, bar.y)

CB_FUNC = CFUNCTYPE(None, py_object)
func = CB_FUNC(foo)

class Bar(object):
    def __init__(self):
        self.x = 1
        self.y = 2

bar = Bar()

lib.foo(func, py_object(bar))

import time
time.sleep(600)
