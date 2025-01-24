#!/usr/bin/env sh

echo $(eww get show)
if [[ $(eww get show) = true ]]
then
    eww update show=false
else
    eww update show=true
fi
