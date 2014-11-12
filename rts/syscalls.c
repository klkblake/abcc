#include "syscalls.h"

long syscall(long n, long a, long b, long c, long d, long e, long f);
#define syscall6(n, a, b, c, d, e, f) syscall(n, (long)(a), (long)(b), (long)(c), (long)(d), (long)(e), (long)(f))
#define syscall5(n, a, b, c, d, e) syscall6(n, a, b, c, d, e, 0)
#define syscall4(n, a, b, c, d) syscall5(n, a, b, c, d, 0)
#define syscall3(n, a, b, c) syscall4(n, a, b, c, 0)
#define syscall2(n, a, b) syscall3(n, a, b, 0)
#define syscall1(n, a) syscall2(n, a, 0)

char mmap_failed[] = "mmap failed\n";
char write_failed[] = "Write failed\n";

void *brk(void *addr) {
	return (void *) syscall1(0x0c, addr);
}

#define PROT_READ     0x01
#define PROT_WRITE    0x02
#define MAP_PRIVATE   0x02
#define MAP_ANONYMOUS 0x20
void *mmap(long length) {
	long res = syscall6(0x09, 0, length, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (res < 0) {
		DIE(mmap_failed);
	}
	return (void *) res;
}

int munmap(void *addr, long length) {
	return syscall2(0x0b, addr, length);
}

long write(int fd, const char *buf, long size) {
	int n = syscall3(0x01, fd, buf, size);
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
	syscall1(0x3c, err);
}
