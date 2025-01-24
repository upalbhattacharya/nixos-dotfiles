#!/usr/bin/env sh

SHOW=$(eww get show)
KEY=$(xev -1 | awk '{gsub(/),/,""); print $23}')
STATE=$(xev -1 | awk '{print $1}')

while true:
do
    echo "$STATE $KEY"
    if [[ $STATE="KeyPress" && $KEY="SUPER_L" ]]
    then
        eww update show=true
    fi

    if [[ $STATE="KeyRelease" && $KEY="SUPER_L" ]]
    then
        eww update show=false
    fi
    sleep 2
done

