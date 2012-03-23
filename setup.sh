#!/bin/sh

function link {

    SRC=$1
    NEW=$2

    if [ -e "$NEW" ] && ! [ -L "$NEW" ]; then
	mv -f $NEW $NEW.bak
    fi

    if ! [ -L "$NEW" ]; then
	ln -s $(pwd)/$SRC $NEW
    fi
}

link emacs/.custom_emacs ~/.custom_emacs
link emacs/.emacs ~/.emacs
link ssh/config ~/.ssh/config
link git/.gitconfig ~/.gitconfig
link git/.git-completion.sh ~/.git-completion.sh
link bash/.bash_jconf ~/.bash_jconf

# bash profile setup
SNIP="[ -f ~/.bash_jconf ] && source ~/.bash_jconf"

if ! grep "$SNIP" ~/.bash_profile >/dev/null; then
    echo $SNIP >> ~/.bash_profile
fi

source ~/.bash_profile
