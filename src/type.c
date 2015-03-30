#include "type.h"

#include <stdlib.h>

internal
u32 type_hash(union type *key) {
	return (u32) ((u64)key / sizeof(union type));
}

DEFINE_MAP_IMPL(union type *, b1, type_ptr_b1, type_hash);
DEFINE_MAP_IMPL(union type *, union type *, type_ptr, type_hash);
