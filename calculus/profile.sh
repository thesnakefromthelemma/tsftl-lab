#!/bin/sh
set -e

flag () {
    case $1 in
        "list") echo "+list";;
        "vector") echo "+vector";;
        "mvector") echo "+mvector";;
        "linear") echo "+linear";;
        "eager") echo "+eager";;
        "lazy") echo "-eager";;
        "boxed") echo "+boxed";;
        "unboxed") echo "-boxed";;
    esac
}

valid () {
    if test $1 = "list" && test $3 = "boxed"; then
        echo "valid"
    elif $(test $1 = "vector" || test $1 = "mvector") && ! $(test $2 = "lazy" && test $3 = "unboxed"); then
        echo "valid"
    elif test $1 = "linear" && test $2 = "eager" && test $3 = "boxed"; then
        echo "valid"
    else
        echo ""
    fi
}

if test $# = 0 || test $1 = "+llvm"; then
    llvm="+llvm"
else
    llvm="-llvm"
fi

for cnt in "list" "vector" "mvector" "linear"; do
    for bng in "eager" "lazy"; do
        for box in "boxed" "unboxed"; do
            if test $(valid $cnt $bng $box); then
                printf "${cnt}, ${bng}, ${box}:\n"
                cabal run -v0 calculus -f $llvm -f $(flag $cnt) -f $(flag $bng) -f $(flag $box) -- "0" "10000" "-1" +RTS -sstderr
            fi
        done
    done
done