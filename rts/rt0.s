/* Written 2011 Nicholas J. Kain, released as Public Domain */
.text
.global _start
_start:
	xor %rbp, %rbp   /* rbp:undefined -> mark as zero 0 (ABI) */
	pop %rdi         /* 1st arg: argc */
	mov %rsp, %rsi   /* 2nd arg: argv */
	andq $-16, %rsp  /* align stack pointer */
	call main
	mov %rax, %rdi
	call exit
1:	jmp 1b
