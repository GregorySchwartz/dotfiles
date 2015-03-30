#!/bin/bash

sxhkd -c /home/gw/.config/sxhkd/sxhkdrc_swm &

xrdb -load ~/.Xresources

exec /usr/bin/swm
