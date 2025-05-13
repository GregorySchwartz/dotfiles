#!/bin/sh

# Kill focused window

target=$(niri msg --json windows | jq '.[] | select(.is_focused) | .pid')
kill -9 $target
