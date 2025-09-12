#!/bin/sh
set -e

# USAGE: <PATH>/profile.sh <NUMBER OF ATTEMPTS> <"" OR "retry">


# (NON-POSIX) HACK FROM STACKEXCHANGE TO EXTRACT PATH TO THIS SCRIPT;
# THEN cdS INTO "$calculus" SO THAT THIS SCRIPT RUNS EVEN OUTSIDE OF "tsftl-lab"

cruder="$(lsof -p $$ -Fn0 | tail -1 | tr -d "\0")"
crude="${cruder#*n}"
calculus="${crude%/*}"

cd "$calculus"


# HELPERS

## SIGNIFY
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

## VALIDATE TYPE/EAGERNESS/BOXEDNESS COMBOS
valid () {
    if test "$1" = "list" && test "$3" = "boxed"; then
        exit 0
    elif { test "$1" = "vector" || test "$1" = "mvector"; } && ! { test "$2" = "lazy" && test "$3" = "unboxed"; }; then
        exit 0
    elif test "$1" = "linear" && test "$3" = "boxed"; then
        exit 0
    else
        exit 1
    fi
}

## SIZE OF INPUT FOR EACH TYPE/EAGERNESS/BOXEDNESS COMBO (AIM FOR 30sec/RUN-120sec/RUN)
ennOf () {
    case "${1}-${2}-${3}" in
        "list-eager-boxed") echo       "50000";;
        "list-lazy-boxed") echo        "50000";;
        "vector-eager-boxed") echo     "50000";;
        "vector-eager-unboxed") echo  "100000";;
        "vector-lazy-boxed") echo      "15000";;
        "mvector-eager-boxed") echo   "100000";;
        "mvector-eager-unboxed") echo "500000";;
        "mvector-lazy-boxed") echo     "15000";;
        "linear-eager-boxed") echo     "50000";;
        "linear-lazy-boxed") echo      "15000";;
        *) exit 1;;
    esac
}


# MAIN

## TOP-LEVEL BINDINGS
if test "$1"; then
    ATTEMPTS="$1" # NUMBER OF TRIES (TO ACCOUNT FOR THAT eventlog2html MIGHT FAIL)
else
    ATTEMPTS=1
fi

if test "retry" = "$2"; then
    RETRY="True" # PROFILE ONLY THOSE TYPE/EAGERNESS/BOXEDNESS COMBOS WITHOUT PROFILES
else
    RETRY=""
fi

## PATH CONFIGURATION
if ! test -d "prof/.tmp"; then
    mkdir -p "prof/.tmp"
fi

printf "" > "prof/.tmp/log.txt"

## MAIN MAIN
for cnt in "list" "vector" "mvector" "linear"; do
    for bng in "eager" "lazy"; do
        for box in "boxed" "unboxed"; do
            if (valid $cnt $bng $box) && { ! test -f "prof/${cnt}-${bng}-${box}.eventlog.html" || ! test "$RETRY"; }; then
                stem="${cnt}-${bng}-${box}"
                enn="$(ennOf $cnt $bng $box)"                
                attempted=0
                while test "$ATTEMPTS" -gt "$attempted"; do
                    attempted=$((attempted+1))
###                 EXECUTE "calculus-exe", DUMP PROFILING TO "${calculus}/docs"
                    stats=$(cabal run --enable-profiling -v0 "calculus-bench" -f "$(flag $cnt)" -f "$(flag $bng)" -f "$(flag $box)" \
                        -- "0" "$enn" "-1" +RTS -s -hy -p -po"prof/.tmp/${stem}" -l -ol"prof/.tmp/${stem}.eventlog" \
                        2>&1)
###                 GENERATE DOCS AND CLEAN UP
                    if msg=$(eventlog2html "prof/.tmp/${stem}.eventlog" 2>&1); then # eventlog2html KEEPS THROWING (REPLICABLE) OUT OF BOUNDS ERRORS ON THE (NONREPLICABLE) EVENTLOGS AND IDK WHY!
                        mv -f "prof/.tmp/${stem}.eventlog.html" "prof/${stem}.eventlog.html"
                        mv -f "prof/.tmp/${stem}.prof" "prof/${stem}.prof"
                        rm "prof/.tmp/${stem}.hp" "prof/.tmp/${stem}.eventlog"
                        break;
                    else
                        printf "eventlog2html FAILED ON %s'S ATTEMPT %s:\n%s\n\n" "$stem" "$attempted" "$msg" >> "prof/.tmp/log.txt"
                    fi
                done
###             IN ANY CASE DISPLAY (MOST RECENT) STATS
                printf "%s, %s, %s; n = %s:\n%s\n\n" "$cnt" "$bng" "$box" "$enn" "$stats"
            fi
        done
    done
done

## REPORT
log=$(cat "prof/.tmp/log.txt")
if test -n "$log"; then
    printf "%s\nERROR LOG:\n\n%s\n\n" "----------" "$log" >&2
fi

rm "prof/.tmp/log.txt"