#!/bin/bash

emacs --daemon
emacsclient -cn

firefox &

# https://www.linuxquestions.org/questions/slackware-14/under-xfce-4-10-~-xmodmap-is-not-working-on-login-4175437978/
xfce4-terminal -e "xmodmap /home/brukar/.Xmodmaprc"

syndaemon -d -i 0.8
