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

PATH="${PATH}:/home/rich/.cabal/bin"
source /usr/share/nvm/init-nvm.sh
