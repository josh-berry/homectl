#!/bin/bash

# Sanity test to make sure our environment is setup correctly.  If not, we load
# ~/.bash_profile, which is expected to do this.
if ! type hc >/dev/null 2>/dev/null; then
    source ~/.bash_profile
fi

source ~/.homectl/common/shell-boot/rc.sh
