#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#emacs --daemon &
dropboxd & 
xcompmgr &
synapse &
#SpiderOak &
mpd &
volumeicon &
/home/rich/scripts/calendar_check.sh &

/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh

#xset +fp /usr/share/fonts/local
#xset fp rehash

exec awesome
#exec i3

#DEFAULT_SESSION=awesome

#case $1 in
#awesome)
#	exec awesome
#	;;
#openbox)
#	exec openbox-session
#	;;
#*)
#	exec $DEFAULT_SESSION
#	;;
#esac

#exec awesome
