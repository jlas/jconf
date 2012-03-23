#!/bin/sh

# path to the jconf directory
export JCONF="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function link {

    SRC=$1
    NEW=$2

    if [ -e "$NEW" ] && ! [ -L "$NEW" ]; then
	mv -f $NEW $NEW.bak
    fi

    if ! [ -L "$NEW" ]; then
	ln -s $JCONF/$SRC $NEW
    fi
}

link emacs/.custom_emacs ~/.custom_emacs
link emacs/.emacs ~/.emacs
link ssh/config ~/.ssh/config
link git/.gitconfig ~/.gitconfig
link git/.git-completion.sh ~/.git-completion.sh
link bash/.dir_colors ~/.dir_colors
link bash/.bash_jconf ~/.bash_jconf


# bash profile setup

function set_snip {
    if ! grep "$1" ~/.bash_profile >/dev/null; then
        echo $1 >> ~/.bash_profile
    fi
}

set_snip "export JCONF=$JCONF"
set_snip "[ -f ~/.bash_jconf ] && source ~/.bash_jconf"

source ~/.bash_profile
