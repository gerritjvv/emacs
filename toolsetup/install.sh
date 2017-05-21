#!/usr/bin/env bash

brew update && brew install dependency-check


#https://github.com/facebook/infer
brew install infer


curl https://raw.githubusercontent.com/simonthum/git-sync/master/git-sync > /usr/local/bin/git-sync
chmod +x /usr/local/bin/git-sync

