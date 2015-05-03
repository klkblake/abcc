typedef __UINT64_TYPE__ u64;
typedef double f64;
typedef __UINT32_TYPE__ b32;

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

#define UNIT 0xdeadc0de00000001
#define VOID 0xdeadc0de00000000

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
