#!/bin/bash

emacs_setup_dir=`pwd`

echo "Creating $HOME/.emacs.d (if needed)"
mkdir -p $HOME/.emacs.d

echo "Creating $HOME/.emacs.d/emacs-setup as link to $emacs_setup_dir"
ln -s $emacs_setup_dir $HOME/.emacs.d/emacs-setup

echo "Creating $HOME/.emacs.d/init.el"
cp init.el $HOME/.emacs.d

echo "Creating $HOME/.emacs.d/Cask"
cp Cask $HOME/.emacs.d

pushd $HOME/.emacs.d
echo "Installing Packages with Cask"
cask install
popd
