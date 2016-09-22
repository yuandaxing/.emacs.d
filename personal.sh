#!/bin/bash
### BEGIN INIT INFO
# description: personal settings
# Provides: functions
# Required-Start:  personal.sh start 
# Required-Stop: personal.sh stop 
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
### END INIT INFO
start()
{
    if pgrep xflux
    then
        echo "start xflux alread"
    else
        ~/.emacs.d/xflux -l 31.19 -g 121.76 -k 5500
    fi
    setxkbmap -option ctrl:nocaps
}

start 

# add source  ~/.emacs.d/personal.sh to .bashrc
