#!/bin/zsh

# Sanity test to make sure our environment is setup correctly.  If not, we load
# ~/.zprofile, which is expected to do this.
if ! type hc >/dev/null 2>/dev/null; then
    source ~/.zprofile
fi

source ~/.homectl/common/shell-boot/rc.sh
