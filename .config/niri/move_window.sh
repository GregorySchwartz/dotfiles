#!/usr/bin/env bash

# Usage: bash move_window.sh "KeePassXC" "keepassxc"

# id=$(niri msg --json windows | jq '.[] | select(.title | test("~")) | .id' | head -n 1)

scratchId="10"
id=$(niri msg --json windows | jq --arg k1 "$1" '.[] | select(.title | test($k1)) | .id' | head -n 1)

if [[ ! -z $id ]]
then
  #idLocation=$(niri msg windows | grep -A 4 "$1" | grep . | tail -n 1 | sed "s/Workspace ID: //")
  idLocation=$(niri msg --json windows | jq --arg k1 "$1" '.[] | select(.title | test($k1)) | .workspace_id' | head -n 1)
  #target=$(niri msg workspaces | grep "\*" | head -n 1 | cut -d' ' -f 3)
  target=$(niri msg --json workspaces | jq '.[] | select(.is_active) | .id')

  # Determine if the window is in the current workspace (send to scratch) or if
  # it is in scratch (send to current workspace).
  if [[ $idLocation -eq $target ]]
  then
    niri msg action move-window-to-floating --id $id
    niri msg action move-window-to-workspace --window-id $id $scratchId
    niri msg action focus-workspace $idLocation
  else
    niri msg action move-window-to-floating --id $id
    niri msg action center-window --id $id
    niri msg action move-window-to-workspace --window-id $id $target
    niri msg action focus-window --id $id
  fi
else
  # Program not running, so run it
  sh -c "$2"
fi
