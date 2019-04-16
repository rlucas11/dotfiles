#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

alias paconky='/home/rich/scripts/paconky.py /home/rich/temp/pacman'
alias audacityfix='env PULSE_LATENCY_MSEC=120 audacity'
alias update='sudo powerpill -Syu'
alias vpn='sudo openconnect --juniper https://vpn.msu.edu'


PATH="${PATH}:/home/rich/.cabal/bin"
source /usr/share/nvm/init-nvm.sh

export LOCATE_PATH="$HOME/var/mlocate.db"
