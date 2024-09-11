[ "$pgrep -f $1 &> /dev/null ; echo $?" ] && pkill $1 || $1 &
