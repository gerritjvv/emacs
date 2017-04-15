#!/usr/bin/env bash


brew install cask

brew cask install emacs

cd ~/.emacs.d/

##ensure profile exits
[ -f "~/.profile" ] || touch ~/.profile

## add EMACS variable for CASK
grep "EMACS" ~/.profile || echo "export EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs" >> ~/.profile


source ~/.profile

cask install


brew install ag

