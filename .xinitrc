if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Correct display
xrandr --output DVI-I-1 --mode 1920x1080 --rate 144 &

# Load Xresources
xrdb -merge ~/.Xresources &

# Delay between button presses
xset r rate 220 &
# No black screen after inactivity
xset -dpms &
xset s off &
# Caps as control
setxkbmap -option ctrl:nocaps &
# Compositor
compton -f -D 2 -c -C -G -o 0.9 --shadow-exclude '!focused || fullscreen' --shadow-exclude-reg 'x25+0-0' &
# Cursor
xsetroot -cursor_name left_ptr &
# Random background each restart
feh --randomize --bg-fill ~/Dropbox/Desktops/* &
# Notifications
twmnd &
# NetworkManager applet
nm-applet &
# Dropbox
dropbox &
# mopidy
mopidy &
# Locking
xscreensaver -no-splash &

# Start xmonad
exec xmonad
