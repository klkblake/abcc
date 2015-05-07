typedef __UINT8_TYPE__ u8;
typedef __UINT32_TYPE__ u32;
typedef __UINT64_TYPE__ u64;
typedef __INT32_TYPE__ s32;
typedef __INT64_TYPE__ s64;
typedef double f64;
typedef _Bool b1;
typedef u32 b32;

typedef double v2df __attribute__((vector_size(16)));

#define true 1
#define false 0
#define NULL ((void *) 0)
#define STRINGIFY_(str) #str
#define STRINGIFY(str) STRINGIFY_(str)
#define noreturn _Noreturn
#define static_assert _Static_assert

extern f64 floor(f64 value);

#ifdef NDEBUG
# define BREAK
#else
# define BREAK __asm__("int3")
#endif

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
#define EXIT_ASSERT_FAILED  65

static inline
s64 try_write(u32 fd, u8 *buf, s64 size) {
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

static void die_syscall(char *syscall, s64 error);

static inline
void write(u32 fd, u8 *buf, s64 size) {
	s64 result = try_write(fd, buf, size);
	if (result < 0) {
		die_syscall("write", result);
	}
}

static inline noreturn
void exit(u8 err) {
	syscall1(SYS_exit, err);
	__builtin_unreachable();
}

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

// See http://research.swtch.com/ftoa for the algorithm
static
void print_f64(f64 value) {
	// The maximum number of digits occurs for the smallest positive number
	// 1/2^e, which has ceil(0.7e) digits. e is 1074 for doubles. Add an
	// extra one for the decimal point.
	u8 buf[753];
	if (value == 0) {
		buf[0] = '0';
		write(1, buf, 1);
		return;
	}
	if (value < 0) {
		buf[0] = '-';
		// XXX This is horrifically inefficient. Add buffered I/O!
		write(1, buf, 1);
		value = -value;
	}
	union fpbits {
		f64 value;
		u64 bits;
	} value_ = {value};
	f64 fr = (union fpbits){.bits = (value_.bits &~ (0x7ffllu << 52)) | ((1023ull - 1) << 52)}.value;
	s32 exp = (s32)((value_.bits >> 52) &~ (1u << 11)) - 1023 + 1;
	s64 v = (s64)(fr * (1ll << 53));
	s32 e = exp - 53;
	u32 n = 0;
	while (v != 0) {
		buf[n++] = v % 10;
		v /= 10;
	}
	for (u32 i = 0; i < n / 2; i++) {
		u8 tmp = buf[i];
		buf[i] = buf[n - i - 1];
		buf[n - i - 1] = tmp;
	}
	for (; e > 0; e--) {
		u32 delta = buf[0] >= 5;
		u32 x = 0;
		for (u32 i = n - 1; i < n; i--) {
			x += 2 * buf[i];
			buf[i + delta] = x % 10;
			x /= 10;
		}
		if (delta) {
			buf[0] = 1;
			n++;
		}
	}
	u32 dp = n;
	for (; e < 0; e++) {
		if (buf[n - 1] % 2 != 0) {
			buf[n++] = 0;
		}
		u32 delta = 0;
		u32 x = 0;
		if (buf[0] < 2) {
			delta = 1;
			x = buf[0];
			n--;
			dp--;
		}
		for (u32 i = 0; i < n; i++) {
			x = x * 10 + buf[i + delta];
			buf[i] = (u8)(x / 2);
			x %= 2;
		}
	}
	for (u32 i = 0; i < dp; i++) {
		buf[i] += '0';
	}
	if (dp != n) {
		for (u32 i = n-1; i >= dp; i--) {
			buf[i + 1] = buf[i] + '0';
		}
		buf[dp] = '.';
		n++;
	}
	write(1, buf, n);
}

static
void printf(char *format, ...) {
	u64 size;
	for (size = 0; format[size]; size++) {
	}
	write(1, (u8 *)format, (s64)size);
}

static noreturn
void die_syscall(char *syscall, s64 result) {
	printf("%s() failed with error code %d\n", syscall, -result);
	exit(EXIT_SYSCALL_FAILED);
}

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

#define UNIT 0xdeadc0de00000001
#define VOID 0xdeadc0de00000000

#define CHUNK_SIZE (3 * 4096)
#define CHUNK_DATA_SIZE CHUNK_SIZE
#define CHUNK_DATA_LENGTH (CHUNK_DATA_SIZE / sizeof(u64))
static_assert(CHUNK_DATA_SIZE / sizeof(Pair)  * sizeof(Pair)  == CHUNK_DATA_SIZE, "Invalid CHUNK_DATA_SIZE");
static_assert(CHUNK_DATA_SIZE / sizeof(Sum)   * sizeof(Sum)   == CHUNK_DATA_SIZE, "Invalid CHUNK_DATA_SIZE");
static_assert(CHUNK_DATA_SIZE / sizeof(Block) * sizeof(Block) == CHUNK_DATA_SIZE, "Invalid CHUNK_DATA_SIZE");

typedef struct chunk {
	u64 data[CHUNK_DATA_LENGTH];
} Chunk;
static_assert(sizeof(Chunk) == CHUNK_SIZE, "Chunk had padding inserted");

typedef struct {
	Chunk *first_chunk;
	void *first_free;
	u64 used;
	u64 num_chunks;
} Pool;

static Pool pools[] = {
	{NULL, NULL, CHUNK_DATA_LENGTH, 0}, // pool for 2 wide structs
	{NULL, NULL, CHUNK_DATA_LENGTH, 0}, // pool for 3 wide structs
};

#define alloc(type) alloc_(&pools[sizeof(type) / sizeof(u64) - 2], sizeof(type) / sizeof(u64))
static
void *alloc_(Pool *pool, u64 size) {
	if (pool->first_free) {
		void *result = pool->first_free;
		pool->first_free = *((void **)pool->first_free);
#ifndef NDEBUG
		u64 *memory = result;
		for (u64 i = 0; i < size; i++) {
			memory[i] = 0xdeadbeefdeadbeef;
		}
#endif
		return result;
	}
	if (pool->used == CHUNK_DATA_LENGTH) {
		pool->first_chunk = mmap(CHUNK_SIZE);
		pool->used = 0;
		pool->num_chunks++;
#ifndef NDEBUG
		u64 *memory = pool->first_chunk->data;
		for (u64 i = 0; i < size; i++) {
			memory[i] = 0xdeadbeefdeadbeef;
		}
#endif
	}
	return &pool->first_chunk->data[pool->used++ * size];
}

static
void free(Pool *pool, void *ptr) {
	*((void **) ptr) = pool->first_free;
	pool->first_free = ptr;
}

static
Value alloc_pair(Value first, Value second) {
	Value result = {.pair = alloc(Pair)};
	*result.pair = (Pair){first, second, 0xcafe1000};
	return result;
}

static
Value alloc_sum(b1 in_left, Value value) {
	Value result = {.sum = alloc(Sum)};
	*result.sum = (Sum){value, 0xcafe2000};
	result.bits |= in_left;
	return result;
}

static
Value alloc_block_direct(BlockFunction function) {
	Value result = {.block = alloc(Block)};
	*result.block = (Block){{.direct = function}, 0xcafe3000};
	result.bits |= BLOCK_DIRECT;
	return result;
}

static
Value alloc_block_composed(Block *xy, Block *yz) {
	Value result = {.block = alloc(Block)};
	*result.block = (Block){{.block_xy = xy, .block_yz = yz}, 0xcafe4000};
	result.bits |= BLOCK_COMPOSED;
	return result;
}

static
Value alloc_block_quote(Value quoted) {
	Value result = {.block = alloc(Block)};
	*result.block = (Block){{.quoted = quoted}, 0xcafe5000};
	result.bits |= BLOCK_QUOTE;
	return result;
}

static
void decref(Value value) {
	// XXX We need type info to do anything here
	(void) value;
}

static
void assert_void(Value value, b32 in_left) {
	if (in_left) {
		printf("Assertion failed, value not void: %x\n", value.bits);
		BREAK;
		exit(EXIT_ASSERT_FAILED);
	}
}

static
Value apply(Block *block, Value value) {
	u64 type = (u64)block & 0x3;
	block = (Block *)((u64)block &~ 0x3ull);
	switch (type) {
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

static Value block_0 (Value input);

static __attribute__((used))
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
	b32 seen_dp = false;
	f64 scale = 1;
	while ((c = *argstr++)) {
		if (c == '.') {
			if (seen_dp) {
				printf("The argument to %s must be an number\n", argv[0]);
				exit(2);
			} else {
				seen_dp = true;
				continue;
			}
		} else if (c < '0' || c > '9') {
			printf("The argument to %s must be an number\n", argv[0]);
			exit(2);
		}
		arg *= 10;
		arg += c - '0';
		if (seen_dp) {
			scale *= 10;
		}
	}
	arg /= scale;
	arg = negate ? -arg : arg;
	Value unit = {.bits = UNIT};
	Value result = block_0(alloc_pair(alloc_pair((Value){.number = arg}, unit), alloc_pair(unit, unit)));
	printf("%f\n", result.pair->first.number);
	print_f64(result.pair->first.pair->first.number);
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

