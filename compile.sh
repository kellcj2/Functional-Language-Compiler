#!/usr/bin/env bash

# TODO set any environment variables here
# e.g. export LD_LIBRARY_PATH=...

if test -z $1
then
    echo "Usage: compile.sh <file>"
else
    name=$(basename $1 .int)
    # TODO call your program here in place of PROGRAM
    ./main.native $1 > $name.s
    gcc -g -o $name.run runtime.c $name.s
fi
