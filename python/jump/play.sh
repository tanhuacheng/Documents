#!/bin/bash

device='3DN4C16804007869'

while [[ 1 ]]; do
    ./screen-cap.sh
    d=$(./dist.py)

    echo $d
    r=$(adb -s $device shell input swipe 200 200 210 210 $d)
    if [[ -n $r ]]; then
        echo "r=$r"
        exit 1
    fi

    sleep 5
    # read
done
