#include "type.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "string.h"

internal
u32 type_hash(union type *key) {
	return (u32) ((u64)key / sizeof(union type));
}

DEFINE_MAP_IMPL(union type *, b1, type_ptr_b1, type_hash);
DEFINE_MAP_IMPL(union type *, union type *, type_ptr, type_hash);
DEFINE_MAP_IMPL(union type *, u64, type_ptr_u64, type_hash);

#define push_cstring(buf, str) push_many(buf, (u8 *)str, sizeof(str) - 1)
internal
void push_many(struct u8_array *buf, u8 *str, usize len) {
	for (usize i = 0; i < len; i++) {
		array_push(buf, str[i]);
	}
}

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
void push_var(struct u8_array *buf, u64 var) {
	u32 len = var_len(var);
	array_bump(buf, len);
	write_var(buf->data + buf->size - len, var, len);
}

internal
void print_type_(union type *type, u32 prec, struct type_ptr_b1_map *seen, struct type_ptr_u64_map *vars, struct u8_array *buf) {
	if (IS_VAR(type)) {
		union type *type_rep = type->rep;
		while (type != type_rep) {
			type = type_rep;
			type_rep = type->rep;
		}
		if (type->terms == NULL) {
			struct type_ptr_u64_map_get_result result = type_ptr_u64_map_get(vars, type);
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
			union type *term = type->terms;
			do {
				// TODO consider printing out the full type
				if (term == type->terms) {
					array_push(buf, ',');
				}
				if (IS_SEALED(term->symbol)) {
					push_cstring(buf, "sealed \" ");
					push_many(buf, term->seal->data, term->seal->size);
					array_push(buf, '"');
				} else {
					switch (term->symbol) {
						case SYMBOL_VOID:    array_push(buf, '0'); break;
						case SYMBOL_UNIT:    array_push(buf, '1'); break;
						case SYMBOL_NUMBER:  array_push(buf, 'N'); break;
						case SYMBOL_PRODUCT: array_push(buf, '*'); break;
						case SYMBOL_SUM:     array_push(buf, '+'); break;
						case SYMBOL_BLOCK:                   push_cstring(buf, "=>"); break;
						case SYMBOL_BLOCK | POLYMORPHIC_BIT: push_cstring(buf, "->"); break;
						default: array_push(buf, '?'); break;
					}
				}
				term = term->next;
			} while (term != type->terms);
			array_push(buf, '}');
			return;
		}
	}
	struct type_ptr_b1_map_get_result seen_result = type_ptr_b1_map_get(seen, type);
	if (seen_result.found) {
		struct type_ptr_u64_map_get_result vars_result = type_ptr_u64_map_get(vars, type);
		if (!vars_result.found) {
			vars_result.value = vars->size;
			type_ptr_u64_map_put_bucket(vars, type, vars_result.value, vars_result.bucket);
		}
		push_var(buf, vars_result.value);
		return;
	}
	type_ptr_b1_map_put_bucket(seen, type, true, seen_result.bucket);
	struct type_ptr_u64_map_get_result vars_result = type_ptr_u64_map_get(vars, type);
	if (vars_result.found) {
		map_delete_bucket(vars, vars_result.bucket);
	}
	usize begin = buf->size;
	if (IS_SEALED(type->symbol)) {
		if (prec > 8) {
			array_push(buf, '(');
		}
		push_cstring(buf, "sealed \"");
		push_many(buf, type->seal->data, type->seal->size);
		push_cstring(buf, "\" ");
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
			push_cstring(buf, " => ");
		} else if (type->symbol == (SYMBOL_BLOCK | POLYMORPHIC_BIT)) {
			push_cstring(buf, " -> ");
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

void print_type(union type *type, struct type_ptr_u64_map *vars) {
	struct type_ptr_b1_map seen = {};
	struct u8_array buf = {};
	print_type_(type, 0, &seen, vars, &buf);
	fwrite(buf.data, 1, buf.size, stdout);
	map_free(&seen);
	array_free(&buf);
}

void print_type_single(union type *type) {
	struct type_ptr_u64_map vars = {};
	print_type(type, &vars);
	map_free(&vars);
}

union type *rep(union type *v) {
	union type *v0 = v->rep;
	while (v0 != v0->rep) {
		v0 = v0->rep;
	}
	while (v->rep != v0) {
		union type *tmp = v->rep;
		v->rep = v0;
		v = tmp;
	}
	return v0;
}

union type *deref(union type *type) {
	if (IS_VAR(type)) {
		type = rep(type);
		if (type->terms) {
			return type->terms;
		}
	}
	return type;
}
