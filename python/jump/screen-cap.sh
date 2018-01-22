#!/bin/bash

device='3DN4C16804007869'

adb -s $device shell screencap -p /sdcard/screen.png
adb -s $device pull /sdcard/screen.png screen/
