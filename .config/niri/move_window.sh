#!/usr/bin/env bash

# Usage: bash move_window.sh "Command" "AppID" "Optional Title"
# Example: bash move_window.sh "KeePassXC" "keepassxc"

scratchId="10"

if [ -z "$3" ]
then
  filterId=".[] | select(.app_id | test(\$k2)) | .id"
  filterWorkspace=".[] | select(.app_id | test(\$k2)) | .workspace_id"
else
  filterId=".[] | select(.app_id | test(\$k2)) | select(.title | test(\$k3)) | .id"
  filterWorkspace=".[] | select(.app_id | test(\$k2)) | select(.title | test(\$k3)) | .workspace_id"
fi

id=$(niri msg --json windows | jq --arg k2 "$2" --arg k3 "$3" "$filterId" | head -n 1)

if [[ ! -z $id ]]
then
  idLocation=$(niri msg --json windows | jq --arg k2 "$2" --arg k3 "$3" "$filterWorkspace" | head -n 1)
  target=$(niri msg --json workspaces | jq '.[] | select(.is_active) | .id')

  # Determine if the window is in the current workspace (send to scratch) or if
  # it is in scratch (send to current workspace).
  if [[ $idLocation -eq $target ]]
  then
    niri msg action move-window-to-floating --id $id
    niri msg action move-window-to-workspace --focus false --window-id $id $scratchId
  else
    niri msg action move-window-to-floating --id $id
    niri msg action move-window-to-workspace --focus false --window-id $id $target
    niri msg action focus-window --id $id
  fi
else
  # Program not running, so run it
  sh -c "$1"

  # Center it
  id=$(niri msg --json windows | jq --arg k1 "$2" '.[] | select(.title | test($k1)) | .id' | head -n 1)
  niri msg action move-window-to-floating --id $id
  niri msg action center-window --id $id
  niri msg action focus-window --id $id
fi
