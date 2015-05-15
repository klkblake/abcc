#include "type.c"

enum uop {
	UOP_START,
	UOP_END,
	UOP_SEAL,
	UOP_UNSEAL,
	UOP_PAIR,
	UOP_UNPAIR,
	UOP_SUM,
	UOP_UNSUM,
	UOP_UNIT_CONSTANT,
	UOP_VOID_CONSTANT,
	UOP_BLOCK_CONSTANT,
	UOP_BOOL_CONSTANT,
	UOP_NUMBER_CONSTANT,
	UOP_TEXT_CONSTANT,
	UOP_COPY,
	UOP_DROP,
	UOP_APPLY,
	UOP_COMPOSE,
	UOP_QUOTE,
	UOP_MARK_RELEVANT,
	UOP_MARK_AFFINE,
	UOP_ADD,
	UOP_MULTIPLY,
	UOP_INVERSE,
	UOP_NEGATE,
	UOP_DIVMOD,
	UOP_AND,
	UOP_OR,
	UOP_NOT,
	UOP_DISTRIB,
	UOP_MERGE,
	UOP_GREATER,
	UOP_ASSERT_COPYABLE,
	UOP_ASSERT_DROPPABLE,
	UOP_ASSERT_NONZERO,
	UOP_ASSERT_VOID,
	UOP_ASSERT_EQUAL,
	UOP_DEBUG_PRINT_RAW,
	UOP_DEBUG_PRINT_TEXT,
};

typedef struct {
	struct Node *node;
	u32 slot;
} InLink;

typedef struct {
	struct Node *node;
	u32 slot;
	u32 link_id;
	Type *type;
} OutLink;

typedef struct InLinkChunk {
	InLink links[3];
	struct InLinkChunk *next;
} InLinkChunk;

typedef struct OutLinkChunk {
	OutLink links[3];
	struct OutLinkChunk *next;
} OutLinkChunk;

typedef struct Node {
	enum uop uop;
	u32 in_count;
	InLinkChunk in;
	u32 out_count;
	OutLinkChunk out;
	union {
		struct Node *next_constant;
		struct Node *left_constant;
	};
	union {
		// All fields should be pointer sized or less, so we can copy
		// whichever is the active one by copying one of the pointers
		// without caring which one is active
		struct Node *right_constant;
		struct Graph *block;
		StringRC *seal;
		StringRC *text;
		f64 number; // XXX should be a rational
		b32 boolean;
	};
	u64 seen;
} Node;
DEFINE_ARRAY(Node *, NodePtr);

typedef struct Graph {
	u32 id;
	Node input;
	Node output;
	Node *constants;

	Pool node_pool;
	Pool in_link_pool;
	Pool out_link_pool;
} Graph;
DEFINE_ARRAY(Graph, graph);

#define IN0(node) ((node)->in.links[0])
#define IN1(node) ((node)->in.links[1])
#define IN2(node) ((node)->in.links[2])

#define OUT0(node) ((node)->out.links[0])
#define OUT1(node) ((node)->out.links[1])
#define OUT2(node) ((node)->out.links[2])
#define OUT3(node) ((node)->out.next->links[0])
#define OUT4(node) ((node)->out.next->links[1])

internal u64 global_traversal = 1;

internal inline
b32 is_constant(u8 uop) {
	return (uop == UOP_UNIT_CONSTANT   ||
	        uop == UOP_VOID_CONSTANT   ||
		uop == UOP_BLOCK_CONSTANT  ||
		uop == UOP_NUMBER_CONSTANT ||
		uop == UOP_TEXT_CONSTANT   ||
		uop == UOP_BOOL_CONSTANT);
}

internal inline
b32 does_implicit_copies(u8 uop) {
	return (uop == UOP_AND ||
	        uop == UOP_OR  ||
	        uop == UOP_NOT);
}
