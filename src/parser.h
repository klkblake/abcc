#ifndef PARSER_H

#include <stdio.h>

#include "array.h"
#include "string.h"
#include "type.h"

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
	// Unlike for other structs, this is not used for memory management
	u32 refcount;
};
DEFINE_ARRAY(struct block *, block_ptr);

void block_free(struct block *block);

struct parse_error {
	struct u8_array line;
	u32 code;
	u32 lineno;
	u32 col;
};
DEFINE_ARRAY(struct parse_error, parse_error);

struct parse_result {
	// NULL if the parse failed
	struct block *block;
	// All the blocks transitively referenced by block, sorted
	// topologically (leaves first)
	struct block_ptr_array blocks;
	struct parse_error_array errors;
};

struct parse_result parse(FILE *stream);
void print_parse_error(struct parse_error error);

#define PARSER_H
#endif
