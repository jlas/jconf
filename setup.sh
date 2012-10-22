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
link emacs/.emacs ~/.emacs
link git/.gitconfig ~/.gitconfig
link git/.git-completion.sh ~/.git-completion.sh
link bash/.dir_colors ~/.dir_colors
link bash/.bash_jconf ~/.bash_jconf

if [ -d ~/.ipython ]; then
    for conf in `ls $JCONF/python/ipython`; do
        link python/ipython/$conf ~/.ipython/$conf
    done
fi

# scripts
if ! [ -d ~/bin ]; then
    mkdir ~/bin
fi

for script in `ls $JCONF/bin`; do
    link bin/$script ~/bin/$script
done

# special ssh setup
link ssh/config ~/.ssh/config
chmod 600 $JCONF/ssh/config

# bash profile setup

function set_snip {
    if ! grep "$1" ~/.bash_profile >/dev/null; then
        echo $1 >> ~/.bash_profile
    fi
}

set_snip "[ -f ~/.bash_jconf ] && source ~/.bash_jconf"

source ~/.bash_profile
