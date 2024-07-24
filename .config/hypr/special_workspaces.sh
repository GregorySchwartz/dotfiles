if [ "$(hyprctl workspaces | grep -A 2 special:$1 | grep windows: | sed 's/windows: //')" -eq 0 ]
then
  hyprctl dispatch -- exec [workspace special:$1 silent] $2
else
  hyprctl dispatch togglespecialworkspace $1
fi
