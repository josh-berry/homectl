#!/bin/bash

homectl-add-path() {
    local v=$1
    shift
    local p

    for p in $@; do
        if [[ -d "$p" ]]; then
            if [[ -z ${!v} ]]; then
                export $v=$p
            elif [[ ${!v} != *$p* ]]; then
                export $v=$p:${!v}
            fi
            # Else already added; do nothing
        fi
    done
}

case "$(uname -s)" in
    Linux)
        homectl-add-libs() {
            homectl-add-path LD_LIBRARY_PATH "$1/lib"
            homectl-add-path LD_LIBRARY_PATH "$1/lib64"
            homectl-add-path LD_LIBRARY_PATH "$1/lib32"
        }
        ;;

    Darwin)
        homectl-add-libs() {
            homectl-add-path DYLD_LIBRARY_PATH "$1/lib"
            homectl-add-path DYLD_FRAMEWORK_PATH "$1/Frameworks"
        }
        ;;

    *)
        add_libs() {
            echo "!!! Don't know how to place $@ in the library path" >&2
        }
        ;;
esac

homectl-load-env() {
    for pkg in "$@"; do
        homectl-add-path PATH "$pkg/bin"
        homectl-add-libs "$pkg"
    done
}

homectl-run-hooks() {
    local hook="$1"
    shift

    for pkg in "$@"; do
        if [[ -e "$pkg/$hook" ]]; then
            source "$pkg/$hook"
        fi
        if [[ -e "$pkg/$hook.sh" ]]; then
            source "$pkg/$hook.sh"
        fi
        if [[ -e "$pkg/$hook.bash" ]]; then
            source "$pkg/$hook.bash"
        fi
    done
}

homectl-load-env ~/.homectl/*
homectl-run-hooks env ~/.homectl/*
homectl-run-hooks shell ~/.homectl/*
