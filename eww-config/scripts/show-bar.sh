#!/usr/bin/env sh

BAR=$(hyprctl layers | grep "eww-bar" | wc -l)
if [[ $BAR -eq 0 ]]; then
    notify-send "Creating bar!" "$BAR"
    eww open-many central-bar left-bar right-bar
fi

