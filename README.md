# emacs
personal emacs config


## Usage

```git clone git@github.com:gerritjvv/emacs.git .emacs.d```

## Install

# MacOSX

install brew

then run the ```install.sh```

## Dependencies

All dependencies are managed using Cask and Pallet.

Pallet mode ensures that any package install done via emacs is added to the
```Cask``` file.

https://github.com/cask/cask

### Ivy

Ivy mode is added to almost everything for searching and narrowing
Counsel is part of ivy and extends its functionality.

### Projectile

Manage projects, helps scope files, e.g open .emacs.d (which is a git repo also) and now all files in emacs.d are treated as a single project.

### Magit

Magic git

### which-key

Show different key options, this is very usefull if you can't remember a key binding and works much better than desc bindings, just type ```C-c c``` wait a second and a list of possible key bindings are shown.

### dash

functional programming in eslips e.g ```-first condp coll```

### ace-window

```C-c n```

Quick jump between windows.

### swiper

```C-s```

Best search

### Ag or Rgrep (Better Grep)

Prefer Rgrep (its faster)
Use Ag as a better grep http://agel.readthedocs.io/en/latest/index.html

ag and ag-counsel provide the best grep functionality and can work per
project or just on files.

install locally with ```brew install ag``` and ```brew install rg```

### Undo tree

Undo stuff with a tree view.

### Company

company quick help https://github.com/expez/company-quickhelp/blob/master/README.md

