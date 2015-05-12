#ifndef GRAPH_H

#include "pool.h"

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

struct in_link {
	struct node *node;
	u32 slot;
};

struct out_link {
	struct node *node;
	u32 slot;
	u32 link_id;
	union type *type;
};

struct in_link_chunk {
	struct in_link links[3];
	struct in_link_chunk *next;
};

struct out_link_chunk {
	struct out_link links[3];
	struct out_link_chunk *next;
};

struct node {
	enum uop uop;
	u32 in_count;
	struct in_link_chunk in;
	u32 out_count;
	struct out_link_chunk out;
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
		b32 boolean;
	};
	u64 seen;
};
DEFINE_ARRAY(struct node *, node_ptr);

struct graph {
	u32 id;
	struct node input;
	struct node output;
	struct node *constants;
};
DEFINE_ARRAY(struct graph, graph);

struct graph_pools {
	struct pool node_pool;
	struct pool in_link_pool;
	struct pool out_link_pool;
};

extern u64 global_traversal;

#define IN0(node) ((node)->in.links[0])
#define IN1(node) ((node)->in.links[1])
#define IN2(node) ((node)->in.links[2])

#define OUT0(node) ((node)->out.links[0])
#define OUT1(node) ((node)->out.links[1])
#define OUT2(node) ((node)->out.links[2])
#define OUT3(node) ((node)->out.next->links[0])
#define OUT4(node) ((node)->out.next->links[1])

struct in_link *in_link(struct node *node, u32 index);
struct out_link *out_link(struct node *node, u32 index);

struct in_link *add_in_link(struct graph_pools *pools, struct node *node);
struct out_link *add_out_link(struct graph_pools *pools, struct node *node);

#define GRAPH_H
#endif
