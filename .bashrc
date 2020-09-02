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
alias b='buku --suggest'

PATH="${PATH}:/home/rich/.cabal/bin"
source /usr/share/nvm/init-nvm.sh

export LOCATE_PATH="$HOME/var/mlocate.db"

source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash
source /usr/share/fzf/fzf-extras.bash

export EDITOR="emacsclient"
export VISUAL="emacsclient -c -a ''"
export ALTERNATE_EDITOR="emacs"
export BROWSER="firefox"


fb() {
    # save newline separated string into an array
    mapfile -t website <<< "$(buku -p -f 5 | column -ts$'\t' | fzf --multi)"

    # open each website
    for i in "${website[@]}"; do
        index="$(echo "$i" | awk '{print $1}')"
        buku -p "$index"
        buku -o "$index"
    done
}
