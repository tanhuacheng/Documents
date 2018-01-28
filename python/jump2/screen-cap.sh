#!/bin/bash

device='d33cc5cb'

mv ./screen/screen.png ./screen/screen-bak.png
adb -s $device shell screencap -p /sdcard/screen.png
adb -s $device pull /sdcard/screen.png screen/
