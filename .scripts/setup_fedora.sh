#! /bin/bash

# Generals
sudo dnf -y install git gh curl lftp

# Text editor
sudo dnf -y install emacs ripgrep zeal

# Development GUI tools
# sudo dnf -y install filezilla poedit

# Media manipulation tools
# sudo dnf -y install inkscape gimp qrencode handbrake ImageMagick GraphicsMagick

# Docker
# sudo dnf -y install docker docker-compose composer
# sudo groupadd docker
# sudo usermod -aG docker $USER
# curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash

# Documents conversion tools
#
# Add this to markdown for Chinese text support.
#
# ---
# mainfont: Noto Sans Mono CJK TC
# ---
# sudo dnf -y install pandoc texlive texlive-latex texlive-xetex \
#      texlive-collection-latex texlive-collection-latexrecommended \
#      texlive-xetex-def texlive-collection-xetex

# sudo dnf -y install qpdf poppler-utils texi2dvi dvipdf
