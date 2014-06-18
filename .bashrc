#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

alias paconky='/home/rich/scripts/paconky.py /home/rich/temp/pacman'

PATH="${PATH}:/home/rich/.cabal/bin"
