typedef __UINT64_TYPE__ u64;
typedef double f64;

typedef union value {
	union value *next;  // for freelist
	struct pair *pair;
	struct sum *sum;    // bottom bit set indicates sum in left
	struct block *block; // bottom two bits indicates type
	f64 number;
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

typedef struct block {
	union {
		Value (*direct)(Value);
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

Value alloc_pair(Value first, Value second);

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
