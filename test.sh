#!/bin/bash
TMPSRC=$(mktemp --suffix=.c)
RTSFILES="
	rts/rt0.s
	rts/syscall.s
	rts/syscalls.c
	rts/rts.c
"

# Make sure to align functions so that we have free bits in the pointer.
./backend/Test > $TMPSRC && gcc -std=c99 -ffreestanding -nostdlib -falign-functions=4 -static -Wall -g -o test -Irts $RTSFILES $TMPSRC
rm $TMPSRC
