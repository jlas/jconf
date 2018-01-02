#!/bin/bash

# path to the jconf directory
export JCONF=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

function link {

    SRC=$1
    NEW=$2

    if [ -e "$NEW" ] && ! [ -L "$NEW" ]; then
	mv -f $NEW $NEW.bak
    fi

    if [ -L "$NEW" ]; then
	rm -rf $NEW
    fi

    ln -s $JCONF/$SRC $NEW
}

link emacs/.custom_emacs ~/.custom_emacs
link emacs/.nxml-mode-20041004 ~/.nxml-mode-20041004
link emacs/.emacs ~/.emacs
link git/.gitconfig ~/.gitconfig
link git/.git-completion.sh ~/.git-completion.sh
link bash/.inputrc ~/.inputrc
link bash/.dir_colors ~/.dir_colors
link bash/.bash_jconf ~/.bash_jconf

# scripts
if ! [ -d ~/bin ]; then
    mkdir ~/bin
fi

for script in `ls $JCONF/bin`; do
    link bin/$script ~/bin/$script
done

# bash profile setup

function set_snip {
    if ! grep "$1" ~/.bash_profile >/dev/null; then
        echo $1 >> ~/.bash_profile
    fi
}

set_snip "[ -f ~/.bash_jconf ] && source ~/.bash_jconf"

source ~/.bash_profile
