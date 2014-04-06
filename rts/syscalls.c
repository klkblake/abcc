#include "syscalls.h"

long syscall();

void *brk(void *addr) {
	return (void *) syscall(0x0c, addr);
}

long write(int fd, const char *buf, long size) {
	int n = syscall(0x01, fd, buf, size);
	if (n == size) {
		return 0;
	}
	if (n >= 0) {
		return write(fd, buf+n, size-n);
	}
	if (n == -EINTR) {
		return write(fd, buf, size);
	}
	return n;
}

void exit(long err) {
	syscall(0x3c, err);
}
