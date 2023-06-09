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

#########
# Kitty #
#########

rm -f $HOME/.config/kitty
ln -s $DOTFILES/.config/kitty/ $HOME/.config

########
# TMUX #
########

rm -f $HOME/.tmux.conf
ln -s $DOTFILES/.tmux.conf $HOME/.tmux.conf
