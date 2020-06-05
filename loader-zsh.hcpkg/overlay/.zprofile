#!/bin/zsh

# This seems to be needed on modern Ubuntu systems which don't do this in their
# system zsh init scripts...
if [ -d /etc/profile.d ]; then
    # This weird find thing is to avoid zsh complaining about not being able to
    # expand globs if there are no files matching the glob.
    for f in $(find /etc/profile.d -maxdepth 1 -name '*.sh' -or -name '*.zsh'); do
        source $f
    done
fi

source ~/.homectl/common/shell-boot/profile.sh
