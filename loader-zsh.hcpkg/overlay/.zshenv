#!/bin/zsh

if [[ -x ~/.homectl/common/bin/hc ]]; then
    export PATH="$(~/.homectl/common/bin/hc path bin PATH)"

    case "$(uname -s)" in
        Linux)
            export LD_LIBRARY_PATH="$(hc path lib LD_LIBRARY_PATH)"
            export LD_LIBRARY_PATH="$(hc path lib32 LD_LIBRARY_PATH)"
            export LD_LIBRARY_PATH="$(hc path lib64 LD_LIBRARY_PATH)"
            ;;
        Darwin)
            export DYLD_LIBRARY_PATH="$(hc path lib DYLD_LIBRARY_PATH)"
            export DYLD_FRAMEWORK_PATH="$(hc path Frameworks DYLD_FRAMEWORK_PATH)"
            ;;
    esac
fi


homectl-run-hooks() {
    local hook="$1"
    shift

    for pkg in $(hc list); do
        if [[ -e "$pkg/$hook" ]]; then
            source "$pkg/$hook"
        fi
        if [[ -e "$pkg/$hook.sh" ]]; then
            source "$pkg/$hook.sh"
        fi
        if [[ -e "$pkg/$hook.zsh" ]]; then
            source "$pkg/$hook.zsh"
        fi
    done
}

homectl-run-hooks env
