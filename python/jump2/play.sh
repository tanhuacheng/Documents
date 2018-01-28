#!/bin/bash

device='d33cc5cb'

while [[ 1 ]]; do
    ./screen-cap.sh
    d=$(./dist-czk.py)

    echo $d
    r=$(adb -s $device shell input swipe 200 200 210 210 $d)
    if [[ -n $r ]]; then
        echo "r=$r"
        exit 1
    fi

    sleep 3
    # read
done
