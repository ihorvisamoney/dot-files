#! /bin/bash

# Update repositories.
sudo apt-get update

# Generals
sudo apt install git curl wget lftp imagemagick silversearcher-ag emacs28 ripgrep zeal tmux emacs28 vlc filezilla

# Text editor
sudo apt install ripgrep zeal

# Docker and docker compose
sudo apt install docker-compose composer
sudo groupadd docker
sudo usermod -aG docker $USER

# Install required PHP extensions
sudo apt install php8.1-bcmath php8.1-curl php8.1-mbstring php8.1-mysql php8.1-tokenizer php8.1-xml php8.1-zip

# System tools
# sudo apt install imagemagick vlc

# PDF tools
sudo apt install poppler-utils qpdf pandoc

# Design tools
sudo apt-get install gimp inkscape handbrake

# Latex tools
sudo apt-get install texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra

# pdflatex: command not found
# Docker
# sudo dnf -y install docker docker-compose composer
# sudo groupadd docker
# sudo usermod -aG docker $USER
# curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
