#ifndef PARSER_H

#include <stdio.h>

#include "array.h"
#include "string.h"
#include "block.h"

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
