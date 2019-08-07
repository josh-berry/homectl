function homectl-run-hooks -a hook \
    -d "Run all .fish files in the named homectl hook directory"

    for f in (hc tree -n $hook '*.fish')
        source "$f"
    end
end

if test -x ~/.homectl/common/bin/hc
    if status is-login
        set -x PATH (~/.homectl/common/bin/hc path bin PATH)
        switch (uname)
            case Linux
                set -x LD_LIBRARY_PATH (hc path lib LD_LIBRARY_PATH)
                set -x LD_LIBRARY_PATH (hc path lib32 LD_LIBRARY_PATH)
                set -x LD_LIBRARY_PATH (hc path lib64 LD_LIBRARY_PATH)
            case Darwin
                set -x DYLD_LIBRARY_PATH (hc path lib DYLD_LIBRARY_PATH)
                set -x DYLD_FRAMEWORK_PATH (
                    hc path Frameworks DYLD_FRAMEWORK_PATH)
        end

        homectl-run-hooks shell-env
    end

    if status is-interactive
        homectl-run-hooks shell-rc
    end
end
