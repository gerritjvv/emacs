#!/usr/bin/env bash

##https://github.com/mooz/percol
#brew install percol # python
#brew install perco #percol but in go so faster
## fzf is faster than percol
https://github.com/junegunn/fzf
brew install fzf

/usr/local/opt/fzf/install

source ~/.bashrc  # bash
source ~/.zshrc   # zsh

#wget https://github.com/changyuheng/zsh-interactive-cd/blob/master/zsh-interactive-cd.plugin.zsh
curl https://raw.githubusercontent.com/changyuheng/zsh-interactive-cd/master/zsh-interactive-cd.plugin.zsh > ~/zsh-interactive-cd.plugin.zsh
echo "source ~/zsh-interactive-cd.plugin.zsh" >> ~/.zshrc
echo "source ~/zsh-interactive-cd.plugin.zsh" >> ~/.profile

source ~/.zshrc

### setup bashrc for macosx

if ! grep -q "source ~/.bashrc" ~/.bash_profile;
then
 echo "source ~/.bashrc" >> ~/.bash_profile
fi
 
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
brew install rg

brew install sbt

brew install editorconfig

