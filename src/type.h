#ifndef TYPE_H

#include "slice.h"

// Sealed values are pointers so they always have the high bit clear
#define HIGH_PTR_BIT (1ull << (sizeof(void *) * 8 - 1))
// Only blocks can be marked polymorphic
#define POLYMORPHIC_BIT  0x8
#define POLYMORPHIC_MASK (~POLYMORPHIC_BIT)

#define SYMBOL_VOID       (HIGH_PTR_BIT | 0)
#define SYMBOL_UNIT       (HIGH_PTR_BIT | 1)
#define SYMBOL_NUMBER     (HIGH_PTR_BIT | 2)
#define SYMBOL_PRODUCT    (HIGH_PTR_BIT | 3)
#define SYMBOL_SUM        (HIGH_PTR_BIT | 4)
#define SYMBOL_BLOCK      (HIGH_PTR_BIT | 5)

#define IS_SEALED(sym) ((sym & HIGH_PTR_BIT) == 0)

/*
 * This type has complicated invariants.
 * If child2/term_count has VAR_BIT set, then it represents a variable, else it
 * represents a term.
 * If symbol/sealer has its high bit set, then it is a normal term, else it is
 * a sealer.
 * next and rep are not guarenteed to be set to sensible values except for when
 * returned from inst(), as they are only used in unify().
 */
union type {
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

#define VAR_BIT (1ull << (sizeof(usize) * 8 - 1))
#define IS_VAR(type) (((type)->term_count & VAR_BIT) != 0)

static_assert(offsetof(union type, child1) == offsetof(union type, term_count), "child1 must be unioned with term_count");

#define TYPE_H
#endif
