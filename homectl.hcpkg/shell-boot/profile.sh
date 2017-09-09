if [ -x ~/.homectl/common/bin/hc ]; then
    PATH=`~/.homectl/common/bin/hc path bin PATH`
    export PATH

    case `uname -s` in
        Linux)
            LD_LIBRARY_PATH=`hc path lib LD_LIBRARY_PATH`
            LD_LIBRARY_PATH=`hc path lib32 LD_LIBRARY_PATH`
            LD_LIBRARY_PATH=`hc path lib64 LD_LIBRARY_PATH`
            export LD_LIBRARY_PATH
            ;;
        Darwin)
            DYLD_LIBRARY_PATH=`hc path lib DYLD_LIBRARY_PATH`
            DYLD_FRAMEWORK_PATH=`hc path Frameworks DYLD_FRAMEWORK_PATH`
            export DYLD_LIBRARY_PATH
            export DYLD_FRAMEWORK_PATH
            ;;
    esac
fi

source ~/.homectl/common/shell-boot/functions.sh
homectl-run-hooks env
