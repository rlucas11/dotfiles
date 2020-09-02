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

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# if [ -s ~/.Xmodmap ]; then 
#     xmodmap ~/.Xmodmap
# fi

export QT_AUTO_SCREEN_SCALE_FACTOR=1
systemctl --user import-environment DISPLAY

#emacs --daemon &
#xcompmgr &
#SpiderOak &
#/home/rich/scripts/calendar_check.sh &
#volumeicon &

updatedb -l 0 -o $HOME/var/mlocate.db -U $HOME &
dropbox & 
#compton --backend glx --glx-no-stencil --vsync opengl-swc --unredir-if-possible --blur-background &
#compton &
picom -b &
# synapse &
mpd &
dunst &
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

#xset +fp /usr/share/fonts/local
#xset fp rehash

#exec awesome
xrandr --dpi 168
exec i3
#exec gnome-session

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
