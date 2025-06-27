#!/bin/sh
set -e

flag () {
    case "$1" in
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
    if test "$1" = "list" && test "$3" = "boxed"; then
        exit 0
    elif (test "$1" = "vector" || test "$1" = "mvector") && ! (test "$2" = "lazy" && test "$3" = "unboxed"); then
        exit 0
    elif test "$1" = "linear" && test "$3" = "boxed"; then
        exit 0
    else
        exit 1
    fi
}

if ! test $# = 1; then
    set "10000"
fi

for cnt in "list" "vector" "mvector" "linear"; do
    for bng in "eager" "lazy"; do
        for box in "boxed" "unboxed"; do
            if (valid $cnt $bng $box); then
                printf "%s, %s, %s:\n" "$cnt" "$bng" "$box"
                cabal run -v0 calculus-exe -f "$(flag $cnt)" -f "$(flag $bng)" -f "$(flag $box)" -- "0" "$1" "-1" +RTS -sstderr
            fi
        done
    done
done