#include "infer_types.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "string.h"
#include "map.h"
#include "type.h"

#define CHUNK_SIZE 4096

internal
union type *alloc_type(struct type_pool *pool) {
	if (pool->used == CHUNK_SIZE || pool->chunks.size == 0) {
		array_push(&pool->chunks, malloc(CHUNK_SIZE * sizeof(union type)));
		pool->used = 0;
	}
	return &pool->chunks.data[pool->chunks.size - 1][pool->used++];
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

#define var() set_var(alloc_type(pool))
internal
union type *set_var(union type *type) {
	type->rep = type;
	type->terms = NULL;
	type->var_count = 1 | VAR_BIT;
	return type;
}

internal
void print_symbol(FILE *out, u64 symbol) {
	if (IS_SEALED(symbol)) {
		struct string_rc *seal = (struct string_rc *) symbol;
		fprintf(out, "seal \"");
		print_string(out, seal);
		putc('"', out);
	} else {
		switch (symbol) {
			case SYMBOL_VOID:    fprintf(out, "void");    break;
			case SYMBOL_UNIT:    fprintf(out, "unit");    break;
			case SYMBOL_NUMBER:  fprintf(out, "number");  break;
			case SYMBOL_PRODUCT: fprintf(out, "product"); break;
			case SYMBOL_SUM:     fprintf(out, "sum");     break;
			case SYMBOL_BLOCK:   fprintf(out, "block");   break;
			case SYMBOL_BLOCK | POLYMORPHIC_BIT: fprintf(out, "polymorphic block"); break;
			default: fprintf(out, "unknown symbol %llx", symbol &~ HIGH_PTR_BIT); break;
		}
	}
}

internal
void print_type_graph(FILE *out, union type *t, u64 id, struct type_ptr_b1_map *seen) {
	struct type_ptr_b1_map_get_result result = type_ptr_b1_map_get(seen, t);
	if (!result.found) {
		type_ptr_b1_map_put_bucket(seen, t, true, result.bucket);
		if (IS_VAR(t)) {
			fprintf(out, "node_%lu_%p [label=\"%llu\"]\n", id, t, t->var_count &~ VAR_BIT);
			fprintf(out, "node_%lu_%p -> node_%lu_%p [label=\"rep\"]\n", id, t, id, t->rep);
			if (t->rep != t) {
				print_type_graph(out, t->rep, id, seen);
			}
			if (t->terms != NULL) {
				fprintf(out, "node_%lu_%p -> node_%lu_%p [label=\"terms\"]\n", id, t, id, t->terms);
				print_type_graph(out, t->terms, id, seen);
			}
		} else {
			fprintf(out, "node_%lu_%p -> node_%lu_%p [label=\"next\"]\n", id, t, id, t->next);
			if (t->next != t) {
				print_type_graph(out, t->next, id, seen);
			}
			if (IS_SEALED(t->symbol)) {
				fprintf(out, "node_%lu_%p [label=\"Sealed: \\\"", id, t);
				print_string(out, t->seal);
				fprintf(out, "\\\"\"]\n");
			} else {
				fprintf(out, "node_%lu_%p [label=\"", id, t);
				print_symbol(out, t->symbol);
				fprintf(out, "\"]\n");
			}
			if (t->symbol != SYMBOL_UNIT && t->symbol != SYMBOL_NUMBER){
				fprintf(out, "node_%lu_%p -> node_%lu_%p [label=\"#0\"]\n", id, t, id, t->child1);
				print_type_graph(out, t->child1, id, seen);
				if (!IS_SEALED(t->symbol)) {
					fprintf(out, "node_%lu_%p -> node_%lu_%p [label=\"#1\"]\n",
					        id, t, id, t->child2);
					print_type_graph(out, t->child2, id, seen);
				}
			}
		}
	}
}

internal
void print_type_graph_root(FILE *out, union type *t, u64 id) {
	struct type_ptr_b1_map seen = {};
	fprintf(out, "subgraph cluster_%lu {\n", id);
	print_type_graph(out, t, id, &seen);
	fprintf(out, "}\n");
	map_free(&seen);
}

static FILE *global_graph_server_out;

internal __attribute__((used))
void init_graph_server(void) {
	pid_t pid = fork();
	if (pid == -1) {
		perror("abcc");
		return;
	}
	if (pid == 0) {
		execlp("java", "java", "-jar", "show-graph/show-graph.jar", "12345", NULL);
		perror("abcc");
		exit(2);
	}
	sleep(1);
	int sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd == -1) {
		perror("abcc");
		return;
	}
	struct sockaddr_in addr = {};
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	addr.sin_port = htons(12345);
	if (connect(sockfd, (struct sockaddr *) &addr, sizeof(struct sockaddr))) {
		perror("abcc");
		return;
	}
	global_graph_server_out = fdopen(sockfd, "w");
	if (!global_graph_server_out) {
		perror("abcc");
		return;
	}
}

internal __attribute__((used))
void graph_server_send(union type *t) {
	print_type_graph_root(global_graph_server_out, t, 1);
	fflush(global_graph_server_out);
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

// TODO Use modified version of Tarjan's SCC algorithm as per
// http://stackoverflow.com/questions/28924321/copying-part-of-a-graph
// TODO Will that work with the requirement for sharing via variables?
internal
union type *inst_copy(union type *type, b32 share_vars, struct type_ptr_map *copied, struct type_pool *pool) {
	if (IS_VAR(type)) {
		type = rep(type);
	}
	struct type_ptr_map_get_result map_result = type_ptr_map_get(copied, type);
	if (map_result.found) {
		return map_result.value;
	}
	if ((IS_VAR(type) && !type->terms && share_vars)  ||
	    type->symbol == SYMBOL_UNIT   ||
	    type->symbol == SYMBOL_NUMBER) {
		type_ptr_map_put_bucket(copied, type, type, map_result.bucket);
		return type;
	}
	union type *new = alloc_type(pool);
	type_ptr_map_put_bucket(copied, type, new, map_result.bucket);
	if (IS_VAR(type)) {
		new->rep = new;
		new->var_count = 1 | VAR_BIT;
		if (type->terms) {
			new->terms = inst_copy(type->terms, share_vars, copied, pool);
		} else {
			new->terms = NULL;
		}
	} else {
		if (type->symbol == (SYMBOL_BLOCK | POLYMORPHIC_BIT)) {
			share_vars = false;
		}
		new->symbol = type->symbol;
		new->next = new;
		new->child1 = NULL; // Set as term
		new->child1 = inst_copy(type->child1, share_vars, copied, pool);
		if (!IS_SEALED(type->symbol)) {
			new->child2 = inst_copy(type->child2, share_vars, copied, pool);
		}
	}
	return new;
}

internal
union type *inst(union type *type, struct type_pool *pool) {
	struct type_ptr_map seen = {};
	union type *result = inst_copy(type, true, &seen, pool);
	map_free(&seen);
	return result;
}

struct unification_error {
	u64 left, right;
};

internal
void print_unification_error(usize i, u8 op, struct unification_error err, union type *left, union type *right) {
	printf("Error on opcode %lu (%c), matching ", i, op);
	print_symbol(stdout, err.left);
	printf(" against ");
	print_symbol(stdout, err.right);
	printf(", while unifying\n\t");
	struct type_ptr_u64_map vars = {};
	print_type(left, &vars);
	printf("\nagainst\n\t");
	print_type(right, &vars);
	map_free(&vars);
	putchar('\n');
}

internal
void add(union type *v, union type *t, struct type_ptr_array *var_stack) {
	union type *t0 = v->terms;
	if (t0 == NULL) {
		v->terms = t;
	} else {
		if (t0->next == t0) {
			array_push(var_stack, v);
		}
		union type *tmp = t0->next;
		t0->next = t->next;
		t->next = tmp;
	}
}

internal
void merge(union type *v1, union type *v2, struct type_ptr_array *var_stack) {
	u64 r1 = v1->var_count &~ VAR_BIT;
	u64 r2 = v2->var_count &~ VAR_BIT;
	union type *bigV, *v;
	if (r1 >= r2) {
		bigV = v1;
		v = v2;
	} else {
		bigV = v2;
		v = v1;
	}
	union type *t0 = v->terms;
	union type *t1 = bigV->terms;
	// This is checking if bigV has at most 1 terms and the sum of the
	// number of terms is at least two
	if ((t1 == NULL && t0 != NULL && t0->next != t0) || (t1 != NULL && t1->next == t1 && t0 != NULL)) {
		array_push(var_stack, bigV);
	}
	if (t1 == NULL) {
		bigV->terms = t0;
	} else if (t0 != NULL) {
		union type *tmp = t0->next;
		t0->next = t1->next;
		t1->next = tmp;
	}
	v->rep = bigV;
	v->terms = NULL;
	v->var_count = VAR_BIT;
	bigV->var_count = r1 + r2 | VAR_BIT;
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
		u64 any = sym;
		sym &= POLYMORPHIC_MASK;
		foreach (term, t_list) {
			if (((*term)->symbol & POLYMORPHIC_MASK) != sym) {
				return (struct unification_error){
					.left =  sym,
					.right = (*term)->symbol,
				};
			}
			all &= (*term)->symbol;
			any |= (*term)->symbol;
		}
		if ((any & POLYMORPHIC_BIT) && !(all & POLYMORPHIC_BIT)) {
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
			for (usize k = 0; k < t_list.size; k++) {
				(&t_list.data[k]->child1)[i] = (&t_list.data[j]->child1)[i];
			}
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
		u64 k = 0;
		if (v->terms != NULL) {
			k = 1;
			union type *term = v->terms;
			while (term->next != v->terms) {
				k++;
				term = term->next;
			}
		}
		if (k >= 2) {
			union type *t_data[k];
			struct type_ptr_array t;
			t.cap = t.size = k;
			t.data = t_data;
			union type *t0 = v->terms;
			for (u64 i = 0; i < k; i++) {
				t.data[i] = t0;
				t0 = t0->next;
				t.data[i]->next = t.data[i];
			}
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
union type *deref(union type *type) {
	if (IS_VAR(type)) {
		type = rep(type);
		if (type->terms) {
			return type->terms;
		}
	}
	return type;
}

internal
void assign(union type *var, union type *term) {
	assert(var->terms == NULL);
	assert(var->rep == var);
	var->terms = term;
}

internal
void assign_term(union type *var, u64 symbol, union type *c1, union type *c2, struct type_pool *pool) {
	union type *term = alloc_type(pool);
	set_term(term, symbol, c1, c2);
	assign(var, term);
}

internal
void assign_sealed(union type *var, struct string_rc *seal, union type *c1, struct type_pool *pool) {
	union type *term = alloc_type(pool);
	set_sealed(term, seal, c1);
	assign(var, term);
}

#if 0
b32 expect_(u8 *pat, union type **input, union type **vars, struct type_pool *pool) {
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
					**locs[i] = *pool->unit;
					*locs[i] = pool->unit;
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
					**locs[i] = *pool->number;
					*locs[i] = pool->number;
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

void print_expect_error(usize i, u8 op, union type* loc, union type *input) {
	printf("Error on opcode %lu (%c), expected product, got ", i, op);
	print_symbol(stdout, loc->symbol);
	putchar('\n');
	print_type_single(input);
	putchar('\n');
}

// TODO be consistent with stdout/stderr
// TODO clean up error reporting
internal
b32 infer_block(struct block *block, struct type_pool *pool) {

#define prod( c1, c2) set_term(alloc_type(pool), SYMBOL_PRODUCT, c1, c2)
#define sum(  c1, c2) set_term(alloc_type(pool), SYMBOL_SUM,     c1, c2)
#define block(c1, c2) set_term(alloc_type(pool), SYMBOL_BLOCK,   c1, c2)
#define sealed(seal, ty) set_sealed(alloc_type(pool), seal, ty)

	block->types = malloc((block->size + 1) * sizeof(union type *));
	block->types[0] = var();
	struct ao_stack_frame *frame = NULL;
	for (usize i = 0, frame_index = 0, block_index = 0, sealer_index = 0; i < block->size; i++) {
		u8 op = block->opcodes[i];
		union type *input = block->types[i];
		union type *output;
		union type *vars[5];
#if 0
#define expect(pat) if (!expect_((u8 *) (pat), &input, vars, pool)) { \
	printf("Error on opcode %lu (%c)\n", i, op); \
	return false; \
}
#endif
#define output(type) output = (type); break
//#define optype(pat, type) expect(pat); output(type)
#define fail() print_backtrace(frame); return false
#define fail_expect(loc) print_expect_error(i, op, loc, input); fail()
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
			case '"':     output(prod(pool->text, input));
			case OP_SEAL: //optype: *vv prod(sealed(block->sealers[sealer_index++], vars[0]), vars[1])
			case OP_UNSEAL:
				{
					struct string_rc *seal = block->sealers[sealer_index++];
					//expect: *vv
					union type *type = deref(vars[0]);
					if (!IS_VAR(type)) {
						if (IS_SEALED(type->symbol)) {
							if (type->seal != seal) {
								printf("Error on opcode %lu (%c), attempted to unseal value sealed with \"", i, op);
								print_string(stdout, type->seal);
								printf("\" using unsealer \"");
								print_string(stdout, seal);
								printf("\"\n");
								fail();
							}
						} else {
							printf("Error on opcode %lu, unsealing non-sealed value ", i);
							print_symbol(stdout, type->symbol);
							printf(" with sealer \"");
							print_string(stdout, seal);
							printf("\"\n");
							fail();
						}
					} else {
						assign_sealed(type, seal, var(), pool);
						type = deref(type);
					}
					output(prod(type->child1, vars[1]));
				}

			// Normal opcodes
			case OP_ASSERT_EQUAL: //optype: *vv input
			case OP_DEBUG_PRINT_TEXT:
			        {
				        if (input == pool->text) {
					        output(input);
					}
					// We use Floyd's "tortoise and hare"
					// algorithm for cycle detection
				        vars[0] = NULL;
					for (union type *tortoise = deref(input);
					     vars[0] != pool->text && vars[0] != tortoise;
					     tortoise = deref(deref(tortoise->child1)->child2), input = vars[0]) {
						//expect: +*N+*Nv11 // Two iterations of the loop
						vars[0] = deref(vars[0]);
						if (IS_VAR(vars[0])) {
							assign(vars[0], pool->text);
							vars[0] = pool->text;
						}
					}
					output(pool->text);
				}

			case 'l': //optype: *v*vv prod(prod(vars[0], vars[1]), vars[2])
			case 'r': //optype: **vvv prod(vars[0], prod(vars[1], vars[2]))
			case 'w': //optype: *v*vv prod(vars[1], prod(vars[0], vars[2]))
			case 'z': //optype: *v*v*vv prod(vars[0], prod(vars[2], prod(vars[1], vars[3])))
			case 'v': output(prod(input, pool->unit));
			case 'c': //optype: *v1 vars[0]
			case '%': //optype: *vv vars[1]
			case '^':
				{
					//expect: *vv
					union type *t = vars[0];
					if (!IS_VAR(t)) {
						t = var();
						t->terms = vars[0];
					}
					output(prod(t, prod(t, vars[1])));
				}

			case '$': // TODO incorporate fixpoint stuff for semi-polymorphic recursion
				{
					//expect: *[vv*vv
					union type *input_term = deref(input);
					union type *b = inst(deref(input_term->child1), pool);
					struct unification_error err = unify(b->child1, inst(vars[2], pool));
					if (err.left != err.right) {
						print_unification_error(i, op, err,
						                        deref(input_term->child1)->child1, vars[2]);
						fail();
					}
					output(prod(b->child2, vars[3]));
				}
			case 'o':
				{
					//expect: *[vv*[vvv
					union type *input_term = deref(input);
					union type *child1_term = deref(input_term->child1);
					union type *child21_term = deref(deref(input_term->child2)->child1);
					union type *b1 = inst(child1_term, pool);
					union type *b2 = inst(child21_term, pool);
					struct unification_error err = unify(b1->child2, b2->child1);
					if (err.left != err.right) {
						union type *left = child1_term->child2;
						union type *right = child21_term->child1;
						print_unification_error(i, op, err, left, right);
						fail();
					}
					union type *result = block(b1->child1, b2->child2);
					output(prod(result, vars[4]));
				}
			case '\'':
				{
					// TODO make this polymorphic iff the value is constant
					//expect: *vv
					union type *s = var();
					(void) s;
					union type *c = deref(vars[0]);
					union type *b = block(s, prod(c, s));
					if (!IS_VAR(c) && (c->symbol == SYMBOL_UNIT ||
					                   c->symbol == SYMBOL_NUMBER ||
					                   c == pool->text)) {
						b ->symbol |= POLYMORPHIC_BIT;
					}
					output(prod(b, vars[1]));
				}
			case 'k':
			case 'f':
				//optype: *[vvv input

			case '#': output(prod(pool->number, input));
			case '0'...'9': //optype: *Nv input

			case '+':
			case '*':
				//optype: *N*Nv deref(input)->child2
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
					union type *child1_term = deref(deref(input)->child1);
					union type *b = inst(child1_term, pool);
					struct unification_error err = unify(b->child1, inst(vars[2], pool));
					if (err.left != err.right) {
						print_unification_error(i, op, err, child1_term->child1, vars[2]);
						fail();
					}
					output(prod(sum(b->child2, vars[3]), vars[4]));
				}
			case 'D':
				{
					//expect: *v*+vvv
					union type *t = vars[0];
					if (!IS_VAR(t)) {
						t = var();
						t->terms = vars[0];
					}
					output(prod(sum(prod(t, vars[1]), prod(t, vars[2])), vars[3]));
				}
			case 'F': //optype: *+*vv*vvv prod(sum(vars[0], vars[2]), prod(sum(vars[1], vars[3]), vars[4]))
			case 'M':
				{
					//expect: *+vvv
					union type *a = inst(vars[0], pool);
					struct unification_error err = unify(a, inst(vars[1], pool));
					if (err.left != err.right) {
						print_unification_error(i, op, err, vars[0], vars[1]);
						fail();
					}
					output(prod(a, vars[2]));
				}
			case 'K': //optype: *+vvv prod(vars[1], vars[2])

			case '>':
				{
					//expect: *N*Nv
					union type *num_pair = prod(pool->number, pool->number);
					output(prod(sum(num_pair, num_pair), vars[0]));
				}

			default:
			          assert(!"Received invalid opcode");
			          return false;
		}
		block->types[i+1] = output;
	}
	return true;
}

b32 infer_types(struct block_ptr_array blocks, struct type_pool *pool) {
	pool->unit   = set_term(alloc_type(pool), SYMBOL_UNIT,   NULL, NULL);
	pool->number = set_term(alloc_type(pool), SYMBOL_NUMBER, NULL, NULL);
	pool->text   = var();
	assign(pool->text, set_term(alloc_type(pool), SYMBOL_SUM,
	                            set_term(alloc_type(pool), SYMBOL_PRODUCT, pool->number, pool->text),
	                            pool->unit));
	foreach (block, blocks) {
		if (!infer_block(*block, pool)) {
			printf("Failed in block %lu\n", block_index);
			return false;
		}
	}
	return true;
}
