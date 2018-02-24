if which hc >/dev/null 2>/dev/null; then
    source ~/.homectl/common/shell-boot/functions.sh
else
    source ~/.homectl/common/shell-boot/profile.sh
fi
homectl-run-hooks rc
