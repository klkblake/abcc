#!/bin/bash
set -e

TMPSRC=$(mktemp --suffix=.c)
RTSFILES="
	rts/rt0.s
	rts/syscall.s
	rts/syscalls.c
	rts/rts.c
"

(cd backend && ghc --make Test)
# Make sure to align functions so that we have free bits in the pointer.
ao abc "$@" | ./backend/Test > $TMPSRC && gcc -std=c99 -msse4.1 -ffreestanding -nostdlib -falign-functions=4 -static -Wall -g -o test -Irts $RTSFILES $TMPSRC
echo Running
./test | hexdump -e '8/8 "%f\t" "\n"'
echo Done
rm $TMPSRC
