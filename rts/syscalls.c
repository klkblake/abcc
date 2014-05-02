#include "syscalls.h"

long syscall();

char mmap_failed[] = "mmap failed\n";
char write_failed[] = "Write failed\n";

void *brk(void *addr) {
	return (void *) syscall(0x0c, addr);
}

#define PROT_READ     0x01
#define PROT_WRITE    0x02
#define MAP_ANONYMOUS 0x20
void *mmap(long length) {
	long res = syscall(0x09, 0, length, PROT_READ | PROT_WRITE, MAP_ANONYMOUS, -1, -1);
	if (res < 0) {
		DIE(mmap_failed);
	}
	return (void *) res;
}

int munmap(void *addr, long length) {
	return syscall(0x0b, addr, length);
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

void write_force(int fd, const char *buf, long size) {
	long res = write(fd, buf, size);
	if (res != 0) {
		DIE(write_failed);
	}
}

void exit(long err) {
	syscall(0x3c, err);
}
