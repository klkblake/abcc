#define EINTR 4

void *brk(void *addr);
long write(int fd, const char *buf, long count);
void exit(long err);
