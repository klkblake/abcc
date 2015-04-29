#ifndef BLOCK_H

#include "abcc.h"
#include "graph.h"

#define OP_FRAME_PUSH       1
#define OP_FRAME_POP        2
#define OP_SEAL             3
#define OP_UNSEAL           4
#define OP_ASSERT_EQUAL     5
#define OP_DEBUG_PRINT_RAW  6
#define OP_DEBUG_PRINT_TEXT 7

struct ao_stack_frame {
	struct ao_stack_frame *next;
	struct string_rc *word;
	struct string_rc *file;
	u32 line;
	u32 refcount;
};

struct block {
	usize size;
	u8 *opcodes;
	union type **types;
	struct ao_stack_frame **frames;
	struct block **blocks;
	struct string_rc **texts;
	struct string_rc **sealers;
	struct graph graph;
	// Unlike for other structs, this is not used for memory management
	u32 refcount;
};
DEFINE_ARRAY(struct block *, block_ptr);

void print_backtrace(struct ao_stack_frame *frame);

void ao_stack_frame_decref(struct ao_stack_frame *frame);

void block_free(struct block *block);

#define BLOCK_H
#endif
