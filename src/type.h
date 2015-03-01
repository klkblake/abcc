#ifndef TYPE_H

#include "slice.h"

#define SYMBOL_VOID    0
#define SYMBOL_UNIT    1
#define SYMBOL_NUMBER  2
#define SYMBOL_PRODUCT 3
#define SYMBOL_SUM     4
#define SYMBOL_BLOCK   5

union type {
	// This node is a var if child2/var_count has its high bit set. We
	// usually don't need to actually bother masking out that bit when
	// working with var_count.
	struct {
		union {
			u64 symbol;
			struct string_rc *seal;
		};
		union type *next;
		union type *child1;
		union type *child2;
	};
	struct {
		union type *rep;
		union type *terms;
		usize term_count;
		usize var_count;
	};
};
DEFINE_SLICE(union type *, type_ptr);

#define VAR_COUNT_BIT (1ll << (sizeof(usize) * 8 - 1))

static_assert(offsetof(union type, child2) == offsetof(union type, var_count), "child2 must be unioned with term_count");

#define TYPE_H
#endif
