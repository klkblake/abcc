#ifndef GRAPH_H

#include "array.h"

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

struct node {
	enum uop uop;
	u32 in_count;
	struct node *in[2];
	u32 src_slot[2];
	u32 out_count;
	struct node *out[4];
	u32 dst_slot[4];
	u32 out_link_id[4];
	union type *output_type[4];
	union {
		struct node *next_constant;
		struct node *left_constant;
	};
	union {
		struct node *right_constant;
		struct graph *block;
		struct string_rc *seal;
		struct string_rc *text;
		f64 number; // XXX should be a rational
	};
	u64 seen;
};
DEFINE_ARRAY(struct node *, node_ptr);

struct graph {
	struct node input;
	struct node output;
	struct node *constants;
};
DEFINE_ARRAY(struct graph, graph);

struct chunk {
	u32 indent;
	struct u8_array text;
	struct chunk *root; // Only filled for chunks with only constant roots
	struct chunk *next;
	struct chunk *next_else;
	u32 depth;
};
DEFINE_ARRAY(struct chunk, chunk);
DEFINE_ARRAY(struct chunk *, chunk_ptr);

extern u64 global_traversal;

#define GRAPH_H
#endif
