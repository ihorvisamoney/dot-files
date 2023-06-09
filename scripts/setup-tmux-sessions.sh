#!/bin/bash

# ------------------------
#  Setup Sessions
# ------------------------

function tmux-setup-emacs-session() {
    if [ -z "$1" ] || [ -z "$2" ]; then
        echo "Please supply the session name and path."
        return
    fi

    # We need to remove the carriage return.
    cd $2
    tmux new-session   -s $1 -n 'Emacs' -d
    tmux send-keys     -t $1:1 'emacs -nw --debug-init' Enter
    tmux new-window    -t $1:2 -n 'Terminal'
    tmux send-keys     -t $1:2 'ls -la' Enter
    tmux select-window -t $1:1
    cd -

    echo "Created session: $1"

    # Take some time to setup session.
    sleep 0.5
}

# ------------------------
#  General Sessions
# ------------------------

HOME_PATH="/home/vernon"

tmux-setup-emacs-session "Home"     "${HOME_PATH}"
tmux-setup-emacs-session "Notes"    "${HOME_PATH}/Notes/"
tmux-setup-emacs-session "Projects" "${HOME_PATH}/Projects/"
