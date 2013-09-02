
emacs_setup_dir=`pwd`

echo "Creating $HOME/.emacs.d (if needed)"
mkdir -p $HOME/.emacs.d

echo "Creating $HOME/.emacs.d/emacs-setup as link to $emacs_setup_dir"
ln -s $emacs_setup_dir $HOME/.emacs.d/emacs-setup

echo "Creating $HOME/.emacs.d/init.el"
echo '(load (concat user-emacs-directory "emacs-setup/emacs-setup.el"))' >> $HOME/.emacs.d/init.el
