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


## for webkit
brew install python
brew linkapps python
brew install pyqt5 --with-python --without-python3

brew linkapps qt5

sudo pip install svn+https://svn.code.sf.net/p/python-xlib/code/trunk/
sudo pip install epc

mkdir ~/.workspace && cd ~/.workspace && git clone https://github.com/qt/qtwebkit.git && cd qtwebkit && git checkout 5.5 

