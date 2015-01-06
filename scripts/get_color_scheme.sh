file=$(gsettings get org.gnome.desktop.background picture-uri | sed -e "s/file:\/\///" -e "s/'//g" -e "s/\n//")

schemer -t=50 -term="urxvt" ${file} > ~/.colors/dynamic_background.txt

xrdb -merge ~/.Xresources
