#!/bin/sh
set -e

# USAGE: <PATH>/profile.sh <NUMBER OF ATTEMPTS> <"" OR "retry">

# (NON-POSIX) HACK FROM STACKEXCHANGE TO EXTRACT PATH TO THIS SCRIPT;
# THEN cdS INTO "$calculus" SO THAT THIS SCRIPT RUNS EVEN OUTSIDE OF "tsftl-lab"

cruder="$(lsof -p $$ -Fn0 | tail -1 | tr -d "\0")"
crude="${cruder#*n}"
gauss="${crude%/*}"

cd "$gauss"

cabal run --enable-profiling -v0 "gauss-bench" -- \
    $1 $2 $3 $4\
    +RTS -s -hy -p -po"prof/bench" -l -ol"prof/bench.eventlog"

eventlog2html prof/bench.eventlog