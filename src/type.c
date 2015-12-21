#include "pool.c"

// Sealed values are pointers so they always have the high bit clear
#define HIGH_PTR_BIT (1ull << (sizeof(void *) * 8 - 1))
// Only blocks can be marked polymorphic
#define POLYMORPHIC_BIT  0x8ull
#define POLYMORPHIC_MASK (~POLYMORPHIC_BIT)

#define SYMBOL_VOID       (HIGH_PTR_BIT | 0)
#define SYMBOL_UNIT       (HIGH_PTR_BIT | 1)
#define SYMBOL_NUMBER     (HIGH_PTR_BIT | 2)
#define SYMBOL_PRODUCT    (HIGH_PTR_BIT | 3)
#define SYMBOL_SUM        (HIGH_PTR_BIT | 4)
#define SYMBOL_BLOCK      (HIGH_PTR_BIT | 5)
// This is not part of the user-visible type system
#define SYMBOL_BOOL       (HIGH_PTR_BIT | 6)

#define IS_SEALED(sym) ((sym & HIGH_PTR_BIT) == 0)

/*
 * This type has complicated invariants.
 * If child1/var_count has VAR_BIT set, then it represents a variable, else it
 * represents a term.
 * If symbol/sealer has its high bit set, then it is a normal term, else it is
 * a sealer.
 */
typedef struct Type {
	union {
	struct {
		union {
			u64 symbol;
			StringRC *seal;
		};
		struct Type *next;
		struct Type *child1;
		struct Type *child2;
	};
	struct {
		struct Type *rep;
		struct Type *terms;
		usize var_count;
	};
	};
	u64 seen;
} Type;
DEFINE_ARRAY(Type *, TypePtr);

static_assert(offsetof(Type, child1) == offsetof(Type, var_count), "child1 must be unioned with var_count");

#define VAR_BIT (1ull << (sizeof(usize) * 8 - 1))
#define IS_VAR(type) (((type)->var_count & VAR_BIT) != 0)

internal inline
u32 type_hash(Type *key) {
	return (u32) ((u64)key / sizeof(Type));
}

DEFINE_MAP(Type *, b1, type_ptr_b1,TypePtrB1, type_hash);
DEFINE_MAP(Type *, u64, type_ptr_u64, TypePtrU64, type_hash);
DEFINE_MAP(Type *, Type *, type_ptr, TypePtr, type_hash);

internal
u32 var_len(u64 var) {
	u32 len = 0;
	do {
		len++;
		var /= 26;
	} while (var != 0);
	return len;
}

internal
void write_var(u8 *buf, u64 var, u32 len) {
	u32 i = len - 1;
	do {
		buf[i--] = 'a' + var % 26;
		var /= 26;
	} while (var != 0);
}

internal
void push_var(U8Array *buf, u64 var) {
	u32 len = var_len(var);
	array_bump(buf, len);
	write_var(buf->data + buf->size - len, var, len);
}

internal
Type *rep(Type *v) {
	Type *v0 = v->rep;
	while (v0 != v0->rep) {
		v0 = v0->rep;
	}
	while (v->rep != v0) {
		Type *tmp = v->rep;
		v->rep = v0;
		v = tmp;
	}
	return v0;
}

internal
Type *deref(Type *type) {
	if (IS_VAR(type)) {
		type = rep(type);
		if (type->terms) {
			return type->terms;
		}
	}
	return type;
}

internal
void print_type_(Type *type, u32 prec, TypePtrB1Map *seen, TypePtrU64Map *vars, U8Array *buf) {
	if (IS_VAR(type)) {
		type = rep(type);
		if (type->terms == NULL) {
			TypePtrU64MapGetResult result = type_ptr_u64_map_get(vars, type);
			if (!result.found) {
				result.value = vars->size;
				type_ptr_u64_map_put_bucket(vars, type, result.value, result.bucket);
			}
			push_var(buf, result.value);
			return;
		} else if (type->terms->next == type->terms) {
			type = type->terms;
		} else {
			array_push(buf, '{');
			Type *term = type->terms;
			do {
				// TODO consider printing out the full type
				if (term == type->terms) {
					array_push(buf, ',');
				}
				if (IS_SEALED(term->symbol)) {
					u8Array_push_cstring(buf, "sealed \" ");
					u8Array_push_many(buf, term->seal->data, term->seal->size);
					array_push(buf, '"');
				} else {
					switch (term->symbol) {
						case SYMBOL_VOID:    array_push(buf, '0'); break;
						case SYMBOL_UNIT:    array_push(buf, '1'); break;
						case SYMBOL_NUMBER:  array_push(buf, 'N'); break;
						case SYMBOL_PRODUCT: array_push(buf, '*'); break;
						case SYMBOL_SUM:     array_push(buf, '+'); break;
						case SYMBOL_BLOCK:                   u8Array_push_cstring(buf, "=>"); break;
						case SYMBOL_BLOCK | POLYMORPHIC_BIT: u8Array_push_cstring(buf, "->"); break;
						case SYMBOL_BOOL: array_push(buf, 'B'); break;
						default: array_push(buf, '?'); break;
					}
				}
				term = term->next;
			} while (term != type->terms);
			array_push(buf, '}');
			return;
		}
	}
	TypePtrB1MapGetResult seen_result = type_ptr_b1_map_get(seen, type);
	if (seen_result.found) {
		TypePtrU64MapGetResult vars_result = type_ptr_u64_map_get(vars, type);
		if (!vars_result.found) {
			vars_result.value = vars->size;
			type_ptr_u64_map_put_bucket(vars, type, vars_result.value, vars_result.bucket);
		}
		push_var(buf, vars_result.value);
		return;
	}
	type_ptr_b1_map_put_bucket(seen, type, true, seen_result.bucket);
	TypePtrU64MapGetResult vars_result = type_ptr_u64_map_get(vars, type);
	if (vars_result.found) {
		map_delete_bucket(vars, vars_result.bucket);
	}
	usize begin = buf->size;
	if (IS_SEALED(type->symbol)) {
		if (prec > 8) {
			array_push(buf, '(');
		}
		u8Array_push_cstring(buf, "sealed \"");
		u8Array_push_many(buf, type->seal->data, type->seal->size);
		u8Array_push_cstring(buf, "\" ");
		print_type_(type->child1, 9, seen, vars, buf);
		if (prec > 8) {
			array_push(buf, ')');
		}
	} else {
		u8 sym = 0;
		b32 children = true;
		u32 newprec = 100;
		b32 block = false;
		switch (type->symbol) {
			case SYMBOL_VOID:    sym = '0'; children = false; break;
			case SYMBOL_UNIT:    sym = '1'; children = false; break;
			case SYMBOL_NUMBER:  sym = 'N'; children = false; break;
			case SYMBOL_PRODUCT: sym = '*'; newprec = 7; break;
			case SYMBOL_SUM:     sym = '+'; newprec = 6; break;
			case SYMBOL_BLOCK:
			case SYMBOL_BLOCK | POLYMORPHIC_BIT:
			                     block = true;
			                     break;
			case SYMBOL_BOOL: sym = 'B'; children = false; break;
			default:
			                     sym = '?';
		}
		if (prec > newprec) {
			array_push(buf, '(');
		} else if (block) {
			array_push(buf, '[');
		}
		if (children) {
			print_type_(type->child1, newprec + 1, seen, vars, buf);
		}
		if (type->symbol == SYMBOL_BLOCK) {
			u8Array_push_cstring(buf, " => ");
		} else if (type->symbol == (SYMBOL_BLOCK | POLYMORPHIC_BIT)) {
			u8Array_push_cstring(buf, " -> ");
		} else {
			if (children) {
				array_push(buf, ' ');
			}
			array_push(buf, sym);
			if (children) {
				array_push(buf, ' ');
			}
		}
		if (children) {
			print_type_(type->child2, newprec, seen, vars, buf);
		}
		if (prec > newprec) {
			array_push(buf, ')');
		} else if (block) {
			array_push(buf, ']');
		}
	}
	map_delete_bucket(seen, seen_result.bucket);
	vars_result = type_ptr_u64_map_get(vars, type);
	if (vars_result.found) {
		u32 shift = sizeof("μ. ") - 1 + var_len(vars_result.value);
		if (prec > 0) {
			shift++;
		}
		array_bump(buf, shift);
		memmove(buf->data + begin + shift, buf->data + begin, buf->size - begin - shift);
		u8 *hole = buf->data + begin;
		if (prec > 0) {
			*hole++ = '(';
		}
		hole = mempcpy(hole, "μ", sizeof("μ") - 1);
		u32 len = var_len(vars_result.value);
		write_var(hole, vars_result.value, len);
		hole += len;
		hole = mempcpy(hole, ". ", sizeof(". ") - 1);
		if (prec > 0) {
			array_push(buf, ')');
		}
	}
}

internal
void print_type(FILE *file, Type *type, TypePtrU64Map *vars) {
	TypePtrB1Map seen = {};
	U8Array buf = {};
	print_type_(type, 0, &seen, vars, &buf);
	fwrite(buf.data, 1, buf.size, file);
	map_free(&seen);
	array_free(&buf);
}

internal
void print_type_single(FILE *file, Type *type) {
	TypePtrU64Map vars = {};
	print_type(file, type, &vars);
	map_free(&vars);
}
