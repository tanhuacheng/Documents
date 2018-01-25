#!/usr/bin/python3
# -*- coding:utf-8

import os
from ctypes import *

lib = cdll.LoadLibrary(os.path.join(os.path.dirname(__file__), 'libtest.so'))

CB_FUNC = CFUNCTYPE(None)

def foo():
    print('py: foo')

print(lib.foo(CB_FUNC(foo)))

import time
time.sleep(600)
