#define EINTR 4

#define DIE(name) do { write(2, name, sizeof(name)-1); exit(1); } while (0)

void *brk(void *addr);
void *mmap(long length);
int munmap(void *addr, long length);
long write(int fd, const char *buf, long count);
void write_force(int fd, const char *buf, long count);
void exit(long err);
