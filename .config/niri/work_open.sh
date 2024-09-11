niri msg action spawn "emacsclient -c -F '((name . \"Emacs Scratchpad\"))'"
niri msg action move-window-to-workspace "Writing"

niri msg action spawn youtube-music
niri msg action move-window-to-workspace "Scratchpad"

niri msg action spawn slack
niri msg action move-window-to-workspace "Scratchpad"

niri msg action spawn "firefox --name discord -P discord --no-remote --new-window https://discord.com/channels/@me"
niri msg action move-window-to-workspace "Web"

niri msg action spawn "keepass"
niri msg action move-window-to-workspace "Scratchpad"

niri msg action spawn "thunderbird"
niri msg action move-window-to-workspace "Scratchpad"
