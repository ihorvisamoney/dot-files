# -*- mode: shell-script -*-

###########
# General #
###########

# Re-source the current file.
alias reload='. ~/.bash_aliases'

# Restore permissions of everything in the current folder.
alias reset-permissions='find . -type f -exec chmod 644 {} \; && find . -type d -exec chmod 755 {} \;'

# Gnome keyboard typing speed.
alias setup-gnome="
gsettings set org.gnome.desktop.interface enable-animations false
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 15
gsettings set org.gnome.desktop.peripherals.keyboard delay 150
gsettings set org.gnome.rhythmbox.podcast download-interval 'manual'
gsettings set org.gnome.desktop.wm.keybindings activate-window-menu ['']
gsettings set org.freedesktop.ibus.panel.emoji hotkey ['']
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-down ['']
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-up ['']
gsettings set org.gnome.desktop.wm.keybindings minimize ['']
gsettings set org.gnome.desktop.wm.keybindings show-desktop ['']
gsettings set org.gnome.desktop.wm.keybindings switch-applications ['']
gsettings set org.gnome.mutter.keybindings switch-monitor ['']
gsettings set org.gnome.settings-daemon.plugins.media-keys help ['']
gsettings set org.gnome.settings-daemon.plugins.media-keys rotate-video-lock-static ['XF86RotationLockToggle']
gsettings set org.gnome.settings-daemon.plugins.media-keys screensaver ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-1 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-10 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-2 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-3 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-4 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-5 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-6 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-7 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-8 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-9 ['']
gsettings set org.gnome.shell.extensions.dash-to-dock shortcut ['']
gsettings set org.gnome.shell.extensions.dash-to-dock shortcut-text ''
gsettings set org.gnome.shell.keybindings focus-active-notification ['']
gsettings set org.gnome.shell.keybindings switch-to-application-1 ['']
gsettings set org.gnome.shell.keybindings switch-to-application-2 ['']
gsettings set org.gnome.shell.keybindings switch-to-application-3 ['']
gsettings set org.gnome.shell.keybindings switch-to-application-4 ['']
gsettings set org.gnome.shell.keybindings switch-to-application-5 ['']
gsettings set org.gnome.shell.keybindings switch-to-application-6 ['']
gsettings set org.gnome.shell.keybindings switch-to-application-7 ['']
gsettings set org.gnome.shell.keybindings switch-to-application-8 ['']
gsettings set org.gnome.shell.keybindings switch-to-application-9 ['']
gsettings set org.gnome.shell.keybindings toggle-application-view ['']
gsettings set org.gnome.shell.keybindings toggle-message-tray ['']
gsettings set org.gnome.shell.keybindings toggle-overview ['']
gsettings set org.freedesktop.ibus.general.hotkey triggers ['']
org.gnome.settings-daemon.plugins.media-keys rotate-video-lock-static ['']
"

#######
# GIT #
#######

alias gl='git log --graph --pretty=format:'\''%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --abbrev-commit'
alias gs='git status'
alias gb='git branch'
alias gco='git checkout '
alias gm='git merge '
alias gr='git rebase '
alias gri='git rebase -i '
alias gra='git rebase --abort'
alias git-set-origin-url='git remote set-url origin '
alias git-show-crlf='git ls-files --eol | grep crlf'

########
# TMUX #
########

# Attaches tmux to a session (example: ta portal).
alias ta='tmux attach'

# Creates a new session.
alias tn='tmux new-session -s'

# Lists all ongoing sessions.
alias tl='tmux list-sessions'

# Kill a session.
alias tk='tmux kill-session -t'

# Kill all sessions.
alias tka='tmux kill-server'

# Setup tmux sessions. (Babashka)
alias ts="~/Dotfiles/scripts/setup-tmux-sessions.sh"

###############################
# Media And File Manipulation #
###############################

# Crop images
# mogrify -crop 1920x840+0+80 ./*.jpg

# ------------------------------------------------------------------------------
# Will resize all images inside a folder. Images that matches the given format
# will be overwritten.
#
# ARGUMENTS:
# - $1 | The format: jpg/png.
# - $2 | The maximum width in pixels.
#
# OUTPUTS: Resized images.
# RETURNS: void
function resize-image() {
    local -r format="${1}"
    local -r width="${2}"
    mogrify -format $format -quality 80 -resize $width -strip *
}

# ------------------------------------------------------------------------------
# Appends a random number to the end of each file matching the given file
# extension.
#
# ARGUMENTS:
# - $1 | The stand-alone file extension. (pdf, jpg, png, ...)
#
# OUTPUTS: void
# RETURNS: void
function files-append-random-number() {
    local -r extension="${1}"

    for i in *.$extension; do
        [ -f "$i" ] || break
        echo ${i%.*}
        local new_name=${i%.*}-$RANDOM
        mv $i ./$new_name.$extension
    done
}

# ------------------------------------------------------------------------------
# Converts all markdown files inside a folder into PDF files using Pandoc.
#
# ARGUMENTS:
#
# OUTPUTS: Resized images.
# RETURNS: void
function markdown-to-pdf() {
    for i in *.md; do
        [ -f "$i" ] || break
        echo ${i%.*}
        pandoc --pdf-engine=xelatex -f markdown-implicit_figures -t pdf ./${i%.*}.md >./${i%.*}.pdf
    done
}

# ------------------------------------------------------------------------------
# Converts a PDF file to JPG images.
#
# ARGUMENTS:
# - $1 | The name of the PDF file.
#
# REQUIRES:
# - brew install popper
#
# OUTPUTS: void
# RETURNS: void
function pdf-to-jpg() {
    pdftoppm -jpeg -rx 200 -ry 200 $1 page
}

# ------------------------------------------------------------------------------
# Marks an invoice as paid.
#
# ARGUMENTS:
# - $1 | The PDF source invoice path.
# - $2 | Should the chop be added, default false.
#
# REQUIRES:
# - brew install qpdf
#
# OUTPUTS: void
# RETURNS: void
function mark-invoice-paid() {
    local -r pdf_source_path=$1
    local -r pdf_file_name=$(basename -s .pdf "$1")
    local -r pdf_result_file_name="${pdf_file_name}-paid.pdf"
    local overlay_marker="/Users/vernon/Documents/paid-marker.pdf"

    local -r signed="$2"
    if [ "$signed" = "true" ]; then
        overlay_marker="/Users/vernon/Documents/paid-marker-signed.pdf"
    fi

    qpdf $pdf_source_path --overlay \
        $overlay_marker \
        --to=z -- "./${pdf_result_file_name}"

}

####################
# Helper Functions #
####################

# ------------------------------------------------------------------------------
# Generates a php script to dump a mysql database via curl.
#
# ARGUMENTS:
# - $1 | The database host name.
# - $2 | The database name.
# - $3 | The database username.
# - $4 | The database user password.
#
# OUTPUTS: Creates a .temp folder in the current path containing the database
# dump script.
function generate-php-mysql-dump-script() {
    # create temp php dump script.
    local php_file_path="./.temp/tmp_db_dump.php"

    mkdir -p "./.temp"

    touch $php_file_path

    echo "<?php" >$php_file_path
    echo "\$dbhost = '$1';" >>$php_file_path
    echo "\$dbname = '$2';" >>$php_file_path
    echo "\$dbuser = '$3';" >>$php_file_path
    echo "\$dbpass = '$4';" >>$php_file_path
    echo "exec(\"mysqldump --user=\$dbuser --password='\$dbpass' --host=\$dbhost \$dbname > ./database.sql\");" >>$php_file_path
}

#########################
# Remote Server Helpers #
#########################

# ------------------------------------------------------------------------------
# Pull files to a remote server.
#
# ARGUMENTS:
# - $1 | The host name.
# - $2 | The source path.
# - $3 | The destination path.
#
# OUTPUTS: void
function remote-pull-files() {
    rsync -auv $1:$2 $3
}

# ------------------------------------------------------------------------------
# Upload files to a remote server.
#
# ARGUMENTS:
# - $1 | The host name.
# - $2 | The source path.
# - $3 | The destination path.
#
# OUTPUTS: void
function remote-push-files() {
    rsync -auv $2 $1:$3
}

# ------------------------------------------------------------------------------
# Download a MYSQL database from remote server.
#
# ARGUMENTS:
# - $1 | The host name.
# - $2 | The MYSQL user name.
# - $3 | The MYSQL user password.
# - $4 | The MYSQL database name.
# - $5 | The local destination of the SQL file.
#
# OUTPUTS: void
function remote-pull-mysql-db() {
    # dump database on server.
    ssh $1 "mkdir -p ~/temp && mysqldump --user=\"$2\" --password=\"$3\" $4 > ~/temp/$4.sql && ls -la ~/temp/"

    # Rsync it down.
    rsync -auv $1:~/temp/$4.sql $5
}

#################################
# WordPress Development Helpers #
#################################

# ------------------------------------------------------------------------------
# Uploads the provided SQL file into the MySQL container. Replaces the main
# database used for the WordPress projects development.
#
# ARGUMENTS:
# - $1 | The MySQL Docker container id.
# - $2 | Relative path to the database file.
#
# OUTPUTS: void
function wordpress-docker-upload-mysql-db() {
    # all local WordPress projects uses the following MySQL passwords.
    local loc_mysql_user="root"
    local loc_mysql_password="my-secret-pw"
    local loc_mysql_database="exampledb"

    # upload database to local running container.
    cat $2 | docker exec -i $1 /usr/bin/mysql --user="$loc_mysql_user" --password="$loc_mysql_password" $loc_mysql_database
}

# ------------------------------------------------------------------------------
# Changes the URL of a WordPress docker instance, using the WordPress CLI.
#
# ARGUMENTS:
# - $1 | The WordPress Docker container id.
# - $2 | The current URL.
# - $3 | The replacement URL.
#
# OUTPUTS: void
function wordpress-docker-replace-url() {
    # install WordPress CLI.
    docker exec -i $1 sh -c "cd /var/www/html && curl -O https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar"
    docker exec -i $1 sh -c "cd /var/www/html && chmod +x wp-cli.phar"
    docker exec -i $1 sh -c "cd /var/www/html && mv wp-cli.phar /usr/local/bin/wp"

    # replace the sites URL.
    docker exec -i $1 sh -c "cd /var/www/html && wp search-replace --all-tables '$2' '$3' --allow-root"
}

# ------------------------------------------------------------------------------
# Installs development plugins and de-activates plugins that might cause
# production interferences.
#
# ARGUMENTS:
# - $1 | The WordPress Docker container id.
# - $2 | The current URL.
# - $3 | The replacement URL.
#
# OUTPUTS: void
# Corrects the permissions of a WordPress container.
# $1, container id.
function wordpress-docker-setup-dev-plugins() {
    # TODO: Allow variadic argument for plugin names.

    # deactivate plugins that might cause production related issues.
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin deactivate autoptimize --allow-root"
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin deactivate wordfence --allow-root"
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin deactivate autoptimize --allow-root"
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin deactivate easy-wp-smtp --allow-root"
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin deactivate wp-mail-smtp --allow-root"
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin deactivate wpcf7-recaptcha --allow-root"
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin deactivate login-recaptcha --allow-root"
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin deactivate wps-hide-login --allow-root"

    # activate development plugins.
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin install disable-emails --activate --allow-root"
    docker exec -i $1 sh -c "cd /var/www/html && wp plugin install query-monitor --activate --allow-root"
}
