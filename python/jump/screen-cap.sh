#!/bin/bash

device='3DN4C16804007869'

mv ./screen/screen.png ./screen/screen-bak.png
adb -s $device shell screencap -p /sdcard/screen.png
adb -s $device pull /sdcard/screen.png screen/
