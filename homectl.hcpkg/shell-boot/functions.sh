if [ ! -z "$ZSH_NAME" ]; then
    homectl-run-hooks() {
        for f in `hc tree "shell-$1" '*.sh' '*.zsh'`; do
            source "$f"
        done
    }
elif [ ! -z "$BASH" ]; then
    homectl-run-hooks() {
        for f in `hc tree "shell-$1" '*.sh' '*.bash'`; do
            source "$f"
        done
    }
else
    homectl-run-hooks() {
        for f in `hc tree "shell-$1" '*.sh'`; do
            source "$f"
        done
    }
fi
