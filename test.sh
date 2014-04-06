#!/bin/bash
TMPSRC=$(mktemp --suffix=.c)
RTSFILES="
	rts/rt0.s
	rts/syscall.s
	rts/syscalls.c
	rts/rts.c
"
./backend/Test > $TMPSRC && gcc -std=c99 -ffreestanding -nostdlib -static -g -o test -Irts $RTSFILES $TMPSRC
rm $TMPSRC
