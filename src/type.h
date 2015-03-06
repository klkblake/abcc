#ifndef TYPE_H

#include "slice.h"

// Sealed values are pointers so they always have the high bit clear
#define HIGH_PTR_BIT (1ull << (sizeof(void *) * 8 - 1))
#define SYMBOL_VOID    (HIGH_PTR_BIT | 0)
#define SYMBOL_UNIT    (HIGH_PTR_BIT | 1)
#define SYMBOL_NUMBER  (HIGH_PTR_BIT | 2)
#define SYMBOL_PRODUCT (HIGH_PTR_BIT | 3)
#define SYMBOL_SUM     (HIGH_PTR_BIT | 4)
#define SYMBOL_BLOCK   (HIGH_PTR_BIT | 5)

#define IS_SEALED(sym) ((sym & HIGH_PTR_BIT) == 0)

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

#define VAR_COUNT_BIT (1ull << (sizeof(usize) * 8 - 1))
#define IS_VAR(type) (((type)->var_count & VAR_COUNT_BIT) != 0)

static_assert(offsetof(union type, child2) == offsetof(union type, var_count), "child2 must be unioned with term_count");

#define TYPE_H
#endif
