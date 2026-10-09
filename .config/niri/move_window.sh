#!/usr/bin/env bash

# Usage: bash move_window.sh "Command" "AppID" "Optional Title"
# Example: bash move_window.sh "KeePassXC" "keepassxc"

if [ -z "$3" ]
then
  filterId=".[] | select(.app_id | test(\$k2)) | .id"
  filterWorkspace=".[] | select(.app_id | test(\$k2)) | .workspace_id"
  arg3="NA"
else
  filterId=".[] | select(.app_id | test(\$k2)) | select(.title | test(\$k3)) | .id"
  filterWorkspace=".[] | select(.app_id | test(\$k2)) | select(.title | test(\$k3)) | .workspace_id"
  arg3="$3"
fi

id=$(niri msg --json windows | jq --arg k2 "$2" --arg k3 "$3" "$filterId" | head -n 1)

if [[ ! -z $id ]]
then
  dynamicIdLocation=$(niri msg --json windows | jq --arg k2 "$2" --arg k3 "$arg3" "$filterWorkspace" | head -n 1)

  # Always move-window-to-monitor to the right output
  # first, then the idx on that output means what we want. Compare by
  # workspace id (globally unique) rather than is_active (one true per
  # output, ambiguous with >1 monitor) / idx.
  focusedId=$(niri msg --json workspaces | jq '.[] | select(.is_focused) | .id')

  if [[ $dynamicIdLocation -eq $focusedId ]]
  then
    scratchOutput=$(niri msg --json workspaces | jq -r '.[] | select(.name == "Scratchpad") | .output')
    scratchIdx=$(niri msg --json workspaces | jq '.[] | select(.name == "Scratchpad") | .idx')
    niri msg action move-window-to-floating --id $id
    niri msg action move-window-to-monitor --id $id "$scratchOutput"
    niri msg action move-window-to-workspace --focus false --window-id $id $scratchIdx
  else
    focusedOutput=$(niri msg --json workspaces | jq -r '.[] | select(.is_focused) | .output')
    target=$(niri msg --json workspaces | jq '.[] | select(.is_focused) | .idx')
    niri msg action move-window-to-floating --id $id
    niri msg action move-window-to-monitor --id $id "$focusedOutput"
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
