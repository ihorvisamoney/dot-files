HOME=/home/vernon
DOTFILES=/home/vernon/Dotfiles

#########
# Emacs #
#########

rm -f $HOME/.emacs
ln -s $DOTFILES/.emacs $HOME/.emacs

rm -f $HOME/.emacs-config
ln -s $DOTFILES/.emacs-config/ $HOME/

################
# Bash Aliases #
################

rm -f $HOME/.bash_aliases
ln -s $DOTFILES/.bash_aliases $HOME/.bash_aliases
