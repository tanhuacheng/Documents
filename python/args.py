#!/usr/bin/python3
# -*- coding:utf-8

'args 和 kwargs'

__author__ = 'tanhc'

def test_var_args (f_arg, *argv):
    print("first normal arg:", f_arg)
    for arg in argv:
        print("another arg through *argv:", arg)

def greet_me (**kwargs):
    for key, value in kwargs.items():
        print("{0} == {1}".format(key, value))

def test_args_kwargs (arg1, arg2, arg3):
    print("arg1:", arg1)
    print("arg2:", arg2)
    print("arg3:", arg3)

if __name__ == '__main__':
    test_var_args('yasoob', 'python', 'eggs', 'test')
    greet_me(name = 'yasoob')
    args = ("tow", 3, 5)
    test_args_kwargs(*args)
    kwargs = {'arg3':3, 'arg2':'two', 'arg1':5}
    test_args_kwargs(**kwargs)
