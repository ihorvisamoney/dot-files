# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bash_aliases" ]; then
	. "$HOME/.bash_aliases"
    fi
fi




# >>> JVM installed by coursier >>>
export JAVA_HOME="/home/vernon/.cache/coursier/arc/https/github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11%252B28/OpenJDK11-jdk_x64_linux_hotspot_11_28.tar.gz/jdk-11+28"
export PATH="$PATH:/home/vernon/.cache/coursier/arc/https/github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11%252B28/OpenJDK11-jdk_x64_linux_hotspot_11_28.tar.gz/jdk-11+28/bin"
# <<< JVM installed by coursier <<<

# >>> coursier install directory >>>
export PATH="$PATH:/home/vernon/.local/share/coursier/bin"
# <<< coursier install directory <<<
