#ifndef INFER_TYPES_H

#include "type.h"
#include "block.h"

struct type_pool {
	struct type_ptr_array chunks;
	usize used;
	union type *unit;
	union type *number;
	union type *text;
	union type *boolean;
};

b32 infer_types(struct block_ptr_array blocks, struct type_pool *pool);

#define INFER_TYPES_H
#endif
