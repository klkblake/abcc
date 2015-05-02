typedef __UINT64_TYPE__ u64;
typedef double f64;

typedef union value {
	union value *next;  // for freelist
	struct pair *pair;
	struct sum *sum;    // bottom bit set indicates sum in left
	union block *block; // bottom two bits indicates type
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
			union block *block_xy;
			union block *block_yz;
		};
		Value quoted;
	}
	u64 refcount;
} Block;

#define UNIT 0xdeadc0de00000001
