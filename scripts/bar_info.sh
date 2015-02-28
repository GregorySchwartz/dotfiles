#!/usr/bin/bash

# Define the clock
Clock() {
        DATE=$(date "+%a %b %d, %T")

        echo -n "$DATE"
}

#Define the battery
Battery() {
        BATPERC=$(acpi --battery | cut -d, -f2)
        echo "$BATPERC"
}

#Define music
Music() {
        music=$(mpc -f "%title% - %album% - %artist%" | head -n 1)
        echo "$music"
}

#Define volume
Volume() {
        volume=$(amixer sget Master | egrep -o "[0-9]+%" | head -n 1)
        echo "$volume"
}

# Print the info

while true; do
    echo "%{F#ebdbb2}%{B#282828} %{c}%{F#83a598}$(Music) %{r}V $(Volume)  %{F#d3869b}B$(Battery)  %{F#8ec07c}$(Clock) "
        sleep 1;
done

#bash bar_info.sh | bar-aint-recursive -p -g 3000x50+100+20 -d -f "PragmataPro:size=11"

