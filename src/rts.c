typedef __UINT8_TYPE__ u8;
typedef __UINT32_TYPE__ u32;
typedef __UINT64_TYPE__ u64;
typedef __INT64_TYPE__ s64;
typedef double f64;
typedef u32 b32;

typedef union value {
	union value *next;  // for freelist
	struct pair *pair;
	struct sum *sum;    // bottom bit set indicates sum in left
	struct block *block; // bottom two bits indicates type
	f64 number;
	b32 boolean;
	u64 bits;
} Value;

typedef struct pair {
	Value first;
	Value second;
	u64 refcount;
} Pair;


typedef struct sum {
	Value value;
	u64 refcount;
} Sum;

typedef enum {
	BLOCK_DIRECT,
	BLOCK_COMPOSED,
	BLOCK_QUOTE,
} BlockType;

typedef Value (*BlockFunction)(Value);

typedef struct block {
	union {
		BlockFunction direct;
		struct {
			struct block *block_xy;
			struct block *block_yz;
		};
		Value quoted;
	};
	u64 refcount;
} Block;

#define true 1
#define false 0
#define NULL ((void *) 0)
#define STRINGIFY(str) #str
#define noreturn _Noreturn

#define UNIT 0xdeadc0de00000001
#define VOID 0xdeadc0de00000000

static inline
s64 syscall6(u64 syscall_number, s64 arg1, s64 arg2, s64 arg3, s64 arg4, s64 arg5, s64 arg6) {
	register s64 result __asm__("rax");
	register u64 number __asm__("rax") = syscall_number;
	register s64 a1 __asm__("rdi") = arg1;
	register s64 a2 __asm__("rsi") = arg2;
	register s64 a3 __asm__("rdx") = arg3;
	register s64 a4 __asm__("r10") = arg4;
	register s64 a5 __asm__("r8") = arg5;
	register s64 a6 __asm__("r9") = arg6;
	__asm__("syscall"
	        : "=r" (result)
	        : "0" (number), "r" (a1), "r" (a2), "r" (a3), "r" (a4), "r" (a5), "r" (a6)
	        : "memory", "rcx", "r11");
	return result;
}

static inline
s64 syscall3(u64 syscall_number, s64 arg1, s64 arg2, s64 arg3) {
	register s64 result __asm__("rax");
	register u64 number __asm__("rax") = syscall_number;
	register s64 a1 __asm__("rdi") = arg1;
	register s64 a2 __asm__("rsi") = arg2;
	register s64 a3 __asm__("rdx") = arg3;
	__asm__("syscall"
	        : "=r" (result)
	        : "0" (number), "r" (a1), "r" (a2), "r" (a3)
	        : "memory", "rcx", "r11");
	return result;
}

static inline
s64 syscall2(u64 syscall_number, s64 arg1, s64 arg2) {
	register s64 result __asm__("rax");
	register u64 number __asm__("rax") = syscall_number;
	register s64 a1 __asm__("rdi") = arg1;
	register s64 a2 __asm__("rsi") = arg2;
	__asm__("syscall"
	        : "=r" (result)
	        : "0" (number), "r" (a1), "r" (a2)
	        : "memory", "rcx", "r11");
	return result;
}

static inline
s64 syscall1(u64 syscall_number, u64 arg1) {
	register s64 result __asm__("rax");
	register u64 number __asm__("rax") = syscall_number;
	register u64 a1 __asm__("rdi") = arg1;
	__asm__("syscall"
	        : "=r" (result)
	        : "0" (number), "r" (a1)
	        : "memory", "rcx", "r11");
	return result;
}

#define SYS_write  0x01
#define SYS_mmap   0x09
#define SYS_munmap 0x0b
#define SYS_exit   0x3c

#define EXIT_SYSCALL_FAILED 64

static inline
s64 write(u32 fd, u8 *buf, s64 size) {
	while (true) {
		s64 result = syscall3(SYS_write, fd, (s64)buf, size);
		if (result == size) {
			return 0;
		}
		if (result < 0) {
			return result;
		}
		buf += result;
		size -= result;
	}
}

static inline noreturn
void exit(u8 err) {
	syscall1(SYS_exit, err);
	__builtin_unreachable();
}

void die_syscall(char *syscall, s64 error);

#define PROT_READ     0x01
#define PROT_WRITE    0x02
#define MAP_PRIVATE   0x02
#define MAP_ANONYMOUS 0x20
static inline
void *mmap(u64 size) {
	s64 result = syscall6(SYS_mmap, (s64)NULL, (s64)size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (result < 0) {
		die_syscall("mmap", result);
	}
	return (void *)result;
}

static inline
void munmap(void *addr, u64 size) {
	s64 result = syscall2(SYS_munmap, (s64)addr, (s64)size);
	if (result < 0) {
		die_syscall("munmap", result);
	}
}

void printf(char *format, ...) {
	u64 size;
	for (size = 0; format[size]; size++) {
	}
	s64 result = write(1, (u8 *)format, (s64)size);
	if (result < 0) {
		die_syscall("write", result);
	}
}

noreturn
void die_syscall(char *syscall, s64 result) {
	printf("%s() failed with error code %d\n", syscall, -result);
	exit(EXIT_SYSCALL_FAILED);
}

Value alloc_pair(Value first, Value second);
Value alloc_sum(b32 in_left, Value value);
Value alloc_block_direct(BlockFunction function);
Value alloc_block_composed(Block *xy, Block *yz);
Value alloc_block_quote(Value quoted);
void decref(Value value);
void assert_void(Value value, b32 in_left);

Value apply(Block *block, Value value) {
	switch ((u64)block & 0x3) {
		case BLOCK_DIRECT:
			return block->direct(value);
		case BLOCK_COMPOSED:
			value = apply(block->block_xy, value);
			return apply(block->block_yz, value);
		case BLOCK_QUOTE:
			return alloc_pair(block->quoted, value);
	}
	__builtin_unreachable();
}

Value block_0 (Value input);

void main(int argc, char **argv) {
	if (argc != 2) {
		printf("%s takes exactly one argument\n", argv[0]);
		exit(2);
	}
	char *argstr = argv[1];
	f64 arg = 0;
	b32 negate = false;
	if (argstr[0] == '-') {
		argstr++;
		negate = true;
	}
	char c;
	while ((c = *argstr++)) {
		if (c < '0' || c > '9') {
			printf("The argument to %s must be an integer\n", argv[0]);
			exit(2);
		}
		arg *= 10;
		arg += c - '0';
	}
	Value unit = {.bits = UNIT};
	Value result = block_0(alloc_pair((Value){.number = arg}, alloc_pair(unit, unit)));
	printf("%f\n", result.pair->first.number);
}

// This was originally written in 2011 by Nicholas J. Kain, and was released
// into the public domain. I retrieved it from the musl source code. I've since
// inlined the call to exit and removed the infinite loop at the end, since the
// exit syscall cannot return in any circumstances.
__asm__(".global _start\n"
        "_start:\n"
        "\txor %rbp, %rbp\n"  // rbp:undefined -> mark as zero 0 (ABI)
        "\tpop %rdi\n"        // 1st arg: argc
        "\tmov %rsp, %rsi\n"  // 2nd arg: argv
        "\tandq $-16, %rsp\n" // align stack pointer
        "\tcall main\n"
        "\tmov %rax, %rdi\n"
        "\tmov $" STRINGIFY(SYS_exit) ", %rax\n"
        "\tsyscall\n");

