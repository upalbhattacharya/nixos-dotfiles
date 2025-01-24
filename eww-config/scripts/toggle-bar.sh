#!/usr/bin/env sh

SHOW=$(eww get show)
if [[ $SHOW = true ]]
then
    eww update show=false
else
    eww update show=true
fi
