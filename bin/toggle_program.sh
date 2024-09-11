[ "pgrep -f $1 &> /dev/null ; echo $?" ] && pkill "$1" || bash -c "$1"
