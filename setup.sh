#!/bin/sh

function link {

    SRC=$1
    NEW=$2

    if [ -e "$NEW" ] && ! [ -L "$NEW" ]; then
	mv -f $NEW $NEW.bak
    fi

    if ! [ -L "$NEW" ]; then
	ln -s $SRC $NEW
    fi
}

link emacs/.custom_emacs ~/.custom_emacs
link emacs/.emacs ~/.emacs
link ssh/config ~/.ssh/config
link git/.gitconfig ~/.gitconfig
