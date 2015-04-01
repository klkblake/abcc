#include "infer_types.h"

#include <stdlib.h>
#include <stdio.h>

#include "string.h"
#include "map.h"
#include "type.h"

#define CHUNK_SIZE 4096
struct types {
	struct type_ptr_array chunks;
	usize used;
	union type *unit;
	union type *number;
	union type *text;
};

internal
union type *alloc_type(struct types *types) {
	if (types->used == CHUNK_SIZE || types->chunks.size == 0) {
		array_push(&types->chunks, malloc(CHUNK_SIZE * sizeof(union type)));
		types->used = 0;
	}
	return &types->chunks.data[types->chunks.size - 1][types->used++];
}

internal
union type *set_term(union type *type, u64 symbol, union type *c1, union type *c2) {
	type->symbol = symbol;
	type->next = type;
	type->child1 = c1;
	type->child2 = c2;
	return type;
}

internal
union type *set_sealed(union type *type, struct string_rc *seal, union type *ty) {
	type->seal = seal;
	type->next = type;
	type->child1 = ty;
	return type;
}

#define var() set_var(alloc_type(types))
internal
union type *set_var(union type *type) {
	type->rep = type;
	type->terms = NULL;
	type->term_count = VAR_BIT;
	type->var_count = 0;
	return type;
}

internal
void print_symbol(u64 symbol) {
	if (IS_SEALED(symbol)) {
		struct string_rc *seal = (struct string_rc *) symbol;
		printf("seal \"");
		print_string(seal);
		putchar('"');
	} else {
		switch (symbol) {
			case SYMBOL_VOID:    printf("void");    break;
			case SYMBOL_UNIT:    printf("unit");    break;
			case SYMBOL_NUMBER:  printf("number");  break;
			case SYMBOL_PRODUCT: printf("product"); break;
			case SYMBOL_SUM:     printf("sum");     break;
			case SYMBOL_BLOCK:   printf("block");   break;
			case SYMBOL_BLOCK | POLYMORPHIC_BIT: printf("polymorphic block"); break;
			default: printf("unknown symbol %llx", symbol &~ HIGH_PTR_BIT); break;
		}
	}
}

internal
void print_type_graph(union type *t, u64 id, struct type_ptr_b1_map *seen) {
	struct type_ptr_b1_map_get_result result = type_ptr_b1_map_get(seen, t);
	if (!result.found) {
		type_ptr_b1_map_put_bucket(seen, t, true, result.bucket);
		if (IS_VAR(t)) {
			printf("node_%lu_%p [label=\"%lu, %llu\"]\n", id, t, t->var_count, t->term_count &~ VAR_BIT);
			printf("node_%lu_%p -> node_%lu_%p [label=\"rep\"]\n", id, t, id, t->rep);
			if (t->rep != t) {
				print_type_graph(t->rep, id, seen);
			}
			if (t->terms != NULL) {
				printf("node_%lu_%p -> node_%lu_%p [label=\"terms\"]\n", id, t, id, t->terms);
				print_type_graph(t->terms, id, seen);
			}
		} else {
			printf("node_%lu_%p -> node_%lu_%p [label=\"next\"]\n", id, t, id, t->next);
			if (t->next != t) {
				print_type_graph(t->next, id, seen);
			}
			if (IS_SEALED(t->symbol)) {
				printf("node_%lu_%p [label=\"Sealed: \\\"", id, t);
				print_string(t->seal);
				printf("\\\"\"]\n");
			} else {
				printf("node_%lu_%p [label=\"", id, t);
				print_symbol(t->symbol);
				printf("\"]\n");
			}
			if (t->symbol != SYMBOL_UNIT && t->symbol != SYMBOL_NUMBER){
				printf("node_%lu_%p -> node_%lu_%p [label=\"#0\"]\n", id, t, id, t->child1);
				print_type_graph(t->child1, id, seen);
				if (!IS_SEALED(t->symbol)) {
					printf("node_%lu_%p -> node_%lu_%p [label=\"#1\"]\n", id, t, id, t->child2);
					print_type_graph(t->child2, id, seen);
				}
			}
		}
	}
}

internal
void print_type_graph_root(union type *t, u64 id) {
	struct type_ptr_b1_map seen = {};
	printf("subgraph cluster_%lu {\n", id);
	print_type_graph(t, id, &seen);
	printf("}\n");
	map_free(&seen);
}

// TODO Use modified version of Tarjan's SCC algorithm as per
// http://stackoverflow.com/questions/28924321/copying-part-of-a-graph
internal
union type *inst_copy(union type *type, b32 share_vars, struct type_ptr_map *copied, struct types *types) {
	struct type_ptr_map_get_result map_result = type_ptr_map_get(copied, type);
	if (map_result.found) {
		if (IS_VAR(map_result.value)) {
			return map_result.value;
		} else {
			// Break potential loops
			union type *result = var();
			result->terms = map_result.value;
			result->term_count = VAR_BIT | 1;
			return result;
		}
	}
	if ((IS_VAR(type) && share_vars)  ||
	    type->symbol == SYMBOL_UNIT   ||
	    type->symbol == SYMBOL_NUMBER) {
		type_ptr_map_put_bucket(copied, type, type, map_result.bucket);
		return type;
	}
	union type *new = alloc_type(types);
	type_ptr_map_put_bucket(copied, type, new, map_result.bucket);
	if (IS_VAR(type)) {
		new->rep = new;
		new->terms = NULL;
		new->term_count = VAR_BIT;
		new->var_count = 1;
		return new;
	}
	if (type->symbol == (SYMBOL_BLOCK | POLYMORPHIC_BIT)) {
		share_vars = false;
	}
	new->symbol = type->symbol;
	new->next = new;
	new->term_count = 0; // Set as term
	new->child1 = inst_copy(type->child1, share_vars, copied, types);
	if (IS_SEALED(type->symbol)) {
		return new;
	}
	new->child2 = inst_copy(type->child2, share_vars, copied, types);
	return new;
}

internal
union type *inst(union type *type, struct types *types) {
	struct type_ptr_map seen = {};
	union type *result = inst_copy(type, true, &seen, types);
	map_free(&seen);
	return result;
}

struct unification_error {
	u64 left, right;
};

internal
void print_unification_error(usize i, u8 op, struct unification_error err, union type *left, union type *right) {
	printf("Error on opcode %lu (%c), matching ", i, op);
	print_symbol(err.left);
	printf(" against ");
	print_symbol(err.right);
	printf(", while unifying\n\t");
	print_type(left);
	printf("\nagainst\n\t");
	print_type(right);
	putchar('\n');
}

internal
void add(union type *v, union type *t, struct type_ptr_array *var_stack) {
	if ((v->term_count &~ VAR_BIT) == 1) {
		array_push(var_stack, v);
	}
	union type *t0 = v->terms;
	if (t0 == NULL) {
		v->terms = t;
		t->next = t;
	} else {
		t->next = t0->next;
		t0->next = t;
	}
	v->term_count++;
}

internal
void merge(union type *v1, union type *v2, struct type_ptr_array *var_stack) {
	u64 r1 = v1->var_count;
	u64 r2 = v2->var_count;
	union type *bigV, *v;
	if (r1 >= r2) {
		bigV = v1;
		v = v2;
	} else {
		bigV = v2;
		v = v1;
	}
	u64 k1 = bigV->term_count &~ VAR_BIT;
	u64 k2 = v->term_count &~ VAR_BIT;
	if (k1 <= 1 && k1 + k2 > 1) {
		array_push(var_stack, bigV);
	}
	union type *t0 = v->terms;
	union type *t1 = bigV->terms;
	if (t1 == NULL) {
		bigV->terms = t0;
	} else if (t0 != NULL) {
		union type *tmp = t0->next;
		t0->next = t1->next;
		t1->next = tmp;
	}
	v->rep = bigV;
	v->terms = NULL;
	v->var_count = 0;
	v->term_count = VAR_BIT;
	bigV->var_count = r1 + r2;
	bigV->term_count = (k1 + k2) | VAR_BIT;
}

internal
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

DEFINE_ARRAY(usize, usize);

internal
struct unification_error commonFrontier(struct type_ptr_array t_list, struct type_ptr_array *var_stack) {
	// TODO benchmark with and without checks for identical nodes
	u64 sym = t_list.data[0]->symbol;
	if (IS_SEALED(sym)) {
		foreach (term, t_list) {
			if ((*term)->symbol != sym) {
				return (struct unification_error){
					.left =  sym,
					.right = (*term)->symbol,
				};
			}
		}
	} else {
		u64 all = sym;
		sym &= POLYMORPHIC_MASK;
		foreach (term, t_list) {
			if (((*term)->symbol & POLYMORPHIC_MASK) != sym) {
				return (struct unification_error){
					.left =  sym,
					.right = (*term)->symbol,
				};
			}
			all &= (*term)->symbol;
		}
		// TODO check whether we need to do this for all, or just for
		// the first.
		if (!(all & POLYMORPHIC_BIT)) {
			foreach (term, t_list) {
				(*term)->symbol &= POLYMORPHIC_MASK;
			}
		}
	}
	u64 a;
	if (IS_SEALED(sym)) {
		a = 1;
	} else if (sym == SYMBOL_UNIT || sym == SYMBOL_NUMBER) {
		a = 0;
	} else {
		a = 2;
	}
	struct type_ptr_array t0_list;
	t0_list.cap = t0_list.size = t_list.size;
	t0_list.data = alloca(t0_list.cap * sizeof(union type *));
	// TODO eliminate these stacks
	usize *s0_backing = alloca(t_list.size * sizeof(usize));
	usize *s1_backing = alloca(t_list.size * sizeof(usize));
	for (usize i = 0; i < a; i++) {
		for (usize j = 0; j < t_list.size; j++) {
			t0_list.data[j] = (&t_list.data[j]->child1)[i];
		}
		struct usize_array s0 = {};
		struct usize_array s1 = {};
		s0.size = s1.size = 0;
		s0.cap = s1.cap = t_list.size;
		s0.data = s0_backing;
		s1.data = s1_backing;
		foreach (term, t0_list) {
			if (IS_VAR(*term)) {
				array_push(&s0, term_index);
			} else {
				array_push(&s1, term_index);
			}
		}
		if (s0.size != 0) {
			usize j = s0.data[0];
			s0.data++;
			s0.size--;
			union type tmp = *t_list.data[0];
			*t_list.data[0] = *t_list.data[j];
			*t_list.data[j] = tmp;
			union type *v = rep(t0_list.data[j]);
			foreach (k, s0) {
				union type *v2 = rep(t0_list.data[*k]);
				if (v != v2) {
					merge(v, v2, var_stack);
				}
			}
			foreach (k, s1) {
				add(v, t0_list.data[*k], var_stack);
			}
		} else {
			struct unification_error err = commonFrontier(t0_list, var_stack);
			if (err.left != err.right) {
				return err;
			}
		}
	}
	return (struct unification_error){};
}

// This uses a slightly modified version of Joxan Jaffar's efficient infinite
// unification algorithm.
internal
struct unification_error unify(union type *left, union type *right) {
	struct type_ptr_array var_stack = {};
	b32 left_term = !IS_VAR(left);
	b32 right_term = !IS_VAR(right);
	struct unification_error err = {};
	if (left_term && right_term) {
		// TODO benchmark with and without
		if (left == right) {
			return (struct unification_error){};
		}
		union type *terms[] = {left, right};
		struct type_ptr_array t_list = {terms, 2, 2};
		err = commonFrontier(t_list, &var_stack);
	} else if (left_term) {
		union type* right_rep = rep(right);
		add(right_rep, left, &var_stack);
	} else if (right_term) {
		union type* left_rep = rep(left);
		add(left_rep, right, &var_stack);
	} else {
		union type* left_rep = rep(left);
		union type* right_rep = rep(right);
		if (left_rep != right_rep) {
			merge(left_rep, right_rep, &var_stack);
		} else {
			return (struct unification_error){};
		}
	}
	if (err.left != err.right) {
		array_free(&var_stack);
		return err;
	}
	while (var_stack.size > 0) {
		union type *v = array_pop(&var_stack);
		u64 k = v->term_count &~ VAR_BIT;
		if (k >= 2) {
			struct type_ptr_array t;
			t.cap = t.size = k;
			t.data = alloca(t.cap * sizeof(union type *));
			union type *t0 = v->terms;
			for (u64 i = 0; i < k; i++) {
				t.data[i] = t0;
				t0 = t0->next;
			}
			t.data[0]->next = t.data[0];
			v->term_count = 1 | VAR_BIT;
			err = commonFrontier(t, &var_stack);
			if (err.left != err.right) {
				array_free(&var_stack);
				return err;
			}
		}
	}
	array_free(&var_stack);
	return (struct unification_error){};
}

internal
void remove_vars_from_term(union type *term, struct type_ptr_b1_map *seen) {
	if (term == NULL || term->symbol == SYMBOL_UNIT || term->symbol == SYMBOL_NUMBER) {
		return;
	}
	struct type_ptr_b1_map_get_result result = type_ptr_b1_map_get(seen, term);
	if (result.found) {
		return;
	}
	type_ptr_b1_map_put_bucket(seen, term, true, result.bucket);
	if (IS_VAR(term->child1)) {
		union type *var = rep(term->child1);
		if (var->terms) {
			term->child1 = var->terms;
			remove_vars_from_term(term->child1, seen);
		}
	}
	if (!IS_SEALED(term->symbol) && IS_VAR(term->child2)) {
		union type *var = rep(term->child2);
		if (var->terms) {
			term->child2 = var->terms;
			remove_vars_from_term(term->child2, seen);
		}
	}
}

internal
void remove_vars(union type **type, struct type_ptr_b1_map *seen) {
	if (IS_VAR(*type)) {
		*type = rep(*type);
		if (!(*type)->terms) {
			return;
		}
		*type = (*type)->terms;
	}
	remove_vars_from_term(*type, seen);
}

#if 0
b32 expect_(u8 *pat, union type **input, union type **vars, struct types *types) {
	union type **locs[8];
	locs[0] = input;
	u32 i = 0;
	u32 v = 0;
	u8 c;
	while ((c = *pat++)) {
		assert(i < 8);
		switch (c) {
			case 'v':
				vars[v++] = *locs[i];
				i--;
				break;
			case '1':
				if (!IS_VAR(*locs[i])) {
					if ((*locs[i])->symbol != SYMBOL_UNIT) {
						// TODO report error
						return false;
					}
				} else {
					**locs[i] = *types->unit;
					*locs[i] = types->unit;
				}
				i--;
				break;
			case 'N':
				if (!IS_VAR(*locs[i])) {
					if ((*locs[i])->symbol != SYMBOL_NUMBER) {
						// TODO report error
						return false;
					}
				} else {
					**locs[i] = *types->number;
					*locs[i] = types->number;
				}
				i--;
				break;
			case '*':
			case '+':
			case '[':
				{
					usize sym;
					switch (c) {
						case '*':
							sym = SYMBOL_PRODUCT;
							break;
						case '+':
							sym = SYMBOL_SUM;
							break;
						case '[':
							sym = SYMBOL_BLOCK;
							break;
						default:
							assert(false);
					}
					if (!IS_VAR(*locs[i])) {
						if ((*locs[i])->symbol != sym) {
							// TODO report error
							return false;
						}
					} else {
						set_term(*locs[i], sym, var(), var());
					}
					locs[i+1] = &(*locs[i])->child1;
					locs[i] = &(*locs[i])->child2;
					i++;
					break;
				}
			default:
				assert(false);
		}
	}
	return true;
}
#endif

internal
void print_all_types(union type **types, usize n) {
	union type *last = NULL;
	for (usize j = 0; j < n; j++) {
		if (types[j] == last) {
			continue;
		}
		last = types[j];
		printf("%lu: ", j);
		print_type(types[j]);
		printf("\n");
	}
}

// TODO be consistent with stdout/stderr
// TODO clean up error reporting
internal
b32 infer_block(struct block *block, struct types *types) {

#define prod( c1, c2) set_term(alloc_type(types), SYMBOL_PRODUCT, c1, c2)
#define sum(  c1, c2) set_term(alloc_type(types), SYMBOL_SUM,     c1, c2)
#define block(c1, c2) set_term(alloc_type(types), SYMBOL_BLOCK,   c1, c2)
#define sealed(seal, ty) set_sealed(alloc_type(types), seal, ty)

	block->types = malloc((block->size + 1) * sizeof(union type *));
	block->types[0] = var();
	struct ao_stack_frame *frame = NULL;
	for (usize i = 0, frame_index = 0, block_index = 0, sealer_index = 0; i < block->size; i++) {
		u8 op = block->opcodes[i];
		union type *input = block->types[i];
		union type *output;
		union type *vars[5];
#if 0
#define expect(pat) if (!expect_((u8 *) (pat), &input, vars, types)) { \
	printf("Error on opcode %lu (%c)\n", i, op); \
	return false; \
}
#endif
#define output(type) output = (type); break
//#define optype(pat, type) expect(pat); output(type)
#define fail() print_backtrace(frame); return false
		switch (op) {
			// Identity opcodes
			case OP_FRAME_PUSH:
				frame = block->frames[frame_index++];
				output(input);
			case OP_FRAME_POP:
				frame = frame->next;
				output(input);
			case OP_DEBUG_PRINT_RAW:
				output(input);

			// Opcodes that need additional state access
			case '[':
				{
					// Blocks are topologically ordered, so we know we have
					// already processed this one
					struct block *b = block->blocks[block_index++];
					union type *bty = block(b->types[0], b->types[b->size]);
					bty->symbol |= POLYMORPHIC_BIT;
					output(prod(bty, input));
				}
			case '"':     output(prod(types->text, input));
			case OP_SEAL: //optype: *vv prod(sealed(block->sealers[sealer_index++], vars[0]), vars[1])
			case OP_UNSEAL:
				{
					struct string_rc *seal = block->sealers[sealer_index++];
					//expect: *vv
					if (!IS_VAR(vars[0])) {
						if (IS_SEALED(vars[0]->symbol)) {
							if (vars[0]->seal != seal) {
								printf("Error on opcode %lu (%c), attempted to unseal value sealed with \"", i, op);
								print_string(vars[0]->seal);
								printf("\" using unsealer \"");
								print_string(seal);
								printf("\"\n");
								fail();
							}
						} else {
							printf("Error on opcode %lu, unsealing non-sealed value ", i);
							print_symbol(vars[0]->symbol);
							printf(" with sealer \"");
							print_string(seal);
							printf("\"\n");
							fail();
						}
					} else {
						set_sealed(vars[0], seal, var());
					}
					output(prod(vars[0]->child1, vars[1]));
				}

			// Normal opcodes
			case OP_ASSERT_EQUAL: //optype: *vv input
			case OP_DEBUG_PRINT_TEXT:
			        {
				        if (input == types->text) {
					        output(input);
					}
				        union type *initial = input;
					// We use Floyd's "tortoise and hare"
					// algorithm for cycle detection
					for (union type *tortoise = input;
					     vars[0] != types->text && vars[0] != tortoise;
					     tortoise = tortoise->child1->child2, input = vars[0]) {
						//expect: +*N+*Nv11 // Two iterations of the loop
						if (IS_VAR(vars[0])) {
							input->child1->child2->child1->child2 = types->text;
							vars[0] = types->text;
						}
					}
					*initial = *types->text;
					block->types[i] = types->text;
					output(types->text);
				}

			case 'l': //optype: *v*vv prod(prod(vars[0], vars[1]), vars[2])
			case 'r': //optype: **vvv prod(vars[0], prod(vars[1], vars[2]))
			case 'w': //optype: *v*vv prod(vars[1], prod(vars[0], vars[2]))
			case 'z': //optype: *v*v*vv prod(vars[0], prod(vars[2], prod(vars[1], vars[3])))
			case 'v': output(prod(input, types->unit));
			case 'c': //optype: *v1 vars[0]
			case '%': //optype: *vv vars[1]
			case '^': //optype: *vv prod(vars[0], prod(vars[0], vars[1]))

			case '$': // TODO incorporate fixpoint stuff for semi-polymorphic recursion
				{
					//expect: *[vv*vv
					union type *b = inst(input->child1, types);
					struct unification_error err = unify(b->child1, inst(vars[2], types));
					if (err.left != err.right) {
						print_unification_error(i, op, err, input->child1->child1, vars[2]);
						fail();
					}
					struct type_ptr_b1_map seen = {};
					remove_vars(&b->child2, &seen);
					remove_vars(&vars[3], &seen);
					map_free(&seen);
					output(prod(b->child2, vars[3]));
				}
			case 'o':
				{
					//expect: *[vv*[vvv
					union type *b1 = inst(input->child1, types);
					union type *b2 = inst(input->child2->child1, types);
					struct unification_error err = unify(b1->child2, b2->child1);
					if (err.left != err.right) {
						print_unification_error(i, op, err, input->child1->child2, input->child2->child1->child1);
						fail();
					}
					struct type_ptr_b1_map seen = {};
					remove_vars(&b1->child1, &seen);
					remove_vars(&b2->child2, &seen);
					remove_vars(&vars[4], &seen);
					map_free(&seen);
					union type *result = block(b1->child1, b2->child2);
					output(prod(result, vars[4]));
				}
			case '\'':
				{
					// TODO make this polymorphic iff the value is constant
					union type *s = var();
					(void) s;
					//optype: *vv prod(block(s, prod(vars[0], s)), vars[1])
				}
			case 'k':
			case 'f':
				//optype: *[vvv input

			case '#': output(prod(types->number, input));
			case '0'...'9': //optype: *Nv input

			case '+':
			case '*':
				//optype: *N*Nv input->child2
			case '/':
			case '-':
				//optype: *Nv input
			case 'Q': //optype: *N*Nv input

			case 'L': //optype: *+v+vvv prod(sum(sum(vars[0], vars[1]), vars[2]), vars[3])
			case 'R': //optype: *++vvvv prod(sum(vars[0], sum(vars[1], vars[2])), vars[3])
			case 'W': //optype: *+v+vvv prod(sum(vars[1], sum(vars[0], vars[2])), vars[3])
			case 'Z': //optype: *+v+v+vvv prod(sum(vars[0], sum(vars[1], sum(vars[2], vars[3]))), vars[4])
			case 'V': //optype: *vv prod(sum(vars[0], var()), vars[1])
			case 'C': //optype: *+vvv prod(vars[0], vars[2])

			case '?':
				{
					//expect: *[vv*+vvv
					union type *b = inst(input->child1, types);
					struct unification_error err = unify(b->child1, inst(vars[2], types));
					if (err.left != err.right) {
						print_unification_error(i, op, err, input->child1->child1, vars[2]);
						fail();
					}
					struct type_ptr_b1_map seen = {};
					remove_vars(&b->child2, &seen);
					remove_vars(&vars[3], &seen);
					remove_vars(&vars[4], &seen);
					map_free(&seen);
					output(prod(sum(b->child2, vars[3]), vars[4]));
				}
			case 'D': //optype: *v*+vvv prod(sum(prod(vars[0], vars[1]), prod(vars[0], vars[2])), vars[3])
			case 'F': //optype: *+*vv*vvv prod(sum(vars[0], vars[2]), prod(sum(vars[1], vars[3]), vars[4]))
			case 'M':
				{
					//expect: *+vvv
					union type *a = inst(vars[0], types);
					struct unification_error err = unify(a, inst(vars[1], types));
					if (err.left != err.right) {
						print_unification_error(i, op, err, vars[0], vars[1]);
						fail();
					}
					struct type_ptr_b1_map seen = {};
					remove_vars(&a, &seen);
					remove_vars(&vars[2], &seen);
					map_free(&seen);
					output(prod(a, vars[2]));
				}
			case 'K': //optype: *+vvv prod(vars[1], vars[2])

			case '>':
				{
					//expect: *N*Nv
					union type *num_pair = prod(types->number, types->number);
					output(prod(sum(num_pair, num_pair), vars[0]));
				}

			default:
			          assert(false);
			          return false;
		}
		block->types[i+1] = output;
	}
	// TODO make this conditional on a command line flag
	//printf(" === INFERRED TYPES ===\n");
	//print_all_types(block->types, block->size + 1);
	return true;
}

b32 infer_types(struct block_ptr_array blocks) {
	struct types types = {};
	types.unit   = set_term(alloc_type(&types), SYMBOL_UNIT,   NULL, NULL);
	types.number = set_term(alloc_type(&types), SYMBOL_NUMBER, NULL, NULL);
	types.text   = set_term(alloc_type(&types), SYMBOL_SUM,
	                        set_term(alloc_type(&types), SYMBOL_PRODUCT, types.number, NULL),
	                        types.unit);
	types.text->child1->child2 = types.text;
	foreach (block, blocks) {
		if (!infer_block(*block, &types)) {
			printf("Failed in block %lu\n", block_index);
			return false;
		}
	}
	foreach (chunk, types.chunks) {
		free(*chunk);
	}
	array_free(&types.chunks);
	return true;
}
