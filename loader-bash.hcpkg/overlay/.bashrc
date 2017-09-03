#!/bin/bash

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
    for f in $(hc tree "shell-$1" '*.sh' '*.bash'); do
        source "$f"
    done
}

homectl-run-hooks env
homectl-run-hooks rc
