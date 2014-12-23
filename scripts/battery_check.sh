#!/bin/bash

BATTINFO=`acpi -b`
if [[ `echo $BATTINFO | grep Discharging` && `echo $BATTINFO | cut -f 5 -d " "` < 00:05:00 ]] ; then
    DISPLAY=:0.0 /usr/bin/notify-send -u critical "Recharge Immediately" "$BATTINFO"
elif [[ `echo $BATTINFO | grep Discharging` && `echo $BATTINFO | cut -f 5 -d " "` < 00:15:00 ]] ; then
    DISPLAY=:0.0 /usr/bin/notify-send -u critical "Start Finding an Outlet Battery" "$BATTINFO"
elif [[ `echo $BATTINFO | grep Discharging` && `echo $BATTINFO | cut -f 5 -d " "` < 00:30:00 ]] ; then
    DISPLAY=:0.0 /usr/bin/notify-send "Low Battery" "$BATTINFO"
elif [[ `echo $BATTINFO | grep Discharging` && `echo $BATTINFO | cut -f 5 -d " "` < 01:00:00 ]] ; then
    DISPLAY=:0.0 /usr/bin/notify-send "Lukewarm Battery" "$BATTINFO"
fi
