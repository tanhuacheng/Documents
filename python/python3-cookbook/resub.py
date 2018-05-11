#!/usr/bin/python3

import re

text = 'Today is 5/9/2018, PyCon starts 3/13/2013.'
print(re.findall(r'(?:\d+/){2}\d+', text))
print(re.sub(r'(\d+)/(\d+)/(\d+)', r'\3-\1-\2', text))
