# -*- mode: shell-script; -*-

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
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Reuse existing ssh-agent. Adapted (slightly) from:
# https://superuser.com/a/1469727 and https://unix.stackexchange.com/a/132117
# Set SSH_AUTH_SOCK env var to a fixed value.
export SSH_AUTH_SOCK="${HOME}/.ssh/ssh-agent.sock"

# Test whether SSH_AUTH_SOCK is valid.
ssh-add -l &>/dev/null

# If not valid, then start ssh-agent using SSH_AUTH_SOCK.
[[ "$?" -ge 2 ]] && ssh-agent -a "${SSH_AUTH_SOCK}" >/dev/null
