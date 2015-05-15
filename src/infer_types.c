#include "peephole.c"

typedef struct {
	TypePtrArray chunks;
	usize used;
	Type *unit;
	Type *number;
	Type *text;
	Type *boolean;
} TypePool;

internal
Type *alloc_type(TypePool *pool) {
	if (pool->used == CHUNK_SIZE || pool->chunks.size == 0) {
		array_push(&pool->chunks, malloc(CHUNK_SIZE * sizeof(Type)));
		pool->used = 0;
	}
	return &pool->chunks.data[pool->chunks.size - 1][pool->used++];
}

internal
Type *set_term(Type *type, u64 symbol, Type *c1, Type *c2) {
	type->symbol = symbol;
	type->next = type;
	type->child1 = c1;
	type->child2 = c2;
	return type;
}

internal
Type *set_sealed(Type *type, StringRC *seal, Type *ty) {
	type->seal = seal;
	type->next = type;
	type->child1 = ty;
	return type;
}

#define var() set_var(alloc_type(pool))
internal
Type *set_var(Type *type) {
	type->rep = type;
	type->terms = NULL;
	type->var_count = 1 | VAR_BIT;
	return type;
}

internal
void print_symbol(FILE *out, u64 symbol) {
	if (IS_SEALED(symbol)) {
		StringRC *seal = (StringRC *) symbol;
		fprintf(out, "seal \"");
		print_string(out, seal);
		fputc('"', out);
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
void print_type_graph(FILE *out, Type *t, u64 id, TypePtrB1Map *seen) {
	TypePtrB1MapGetResult result = type_ptr_b1_map_get(seen, t);
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
void print_type_graph_root(FILE *out, Type *t, u64 id) {
	TypePtrB1Map seen = {};
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
void graph_server_send(Type *t) {
	print_type_graph_root(global_graph_server_out, t, 1);
	fflush(global_graph_server_out);
}

// TODO Use modified version of Tarjan's SCC algorithm as per
// http://stackoverflow.com/questions/28924321/copying-part-of-a-graph
// TODO Will that work with the requirement for sharing via variables?
internal
Type *inst_copy(Type *type, b32 share_vars, TypePtrMap *copied, TypePool *pool) {
	if (IS_VAR(type)) {
		type = rep(type);
	}
	TypePtrMapGetResult map_result = type_ptr_map_get(copied, type);
	if (map_result.found) {
		return map_result.value;
	}
	if ((IS_VAR(type) && !type->terms && share_vars)  ||
	    type->symbol == SYMBOL_UNIT   ||
	    type->symbol == SYMBOL_NUMBER) {
		type_ptr_map_put_bucket(copied, type, type, map_result.bucket);
		return type;
	}
	Type *new = alloc_type(pool);
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
Type *inst(Type *type, TypePool *pool) {
	TypePtrMap seen = {};
	Type *result = inst_copy(type, true, &seen, pool);
	map_free(&seen);
	return result;
}

typedef struct {
	u64 left, right;
} UnificationError;

internal
void print_unification_error(usize i, u8 op, UnificationError err, Type *left, Type *right) {
	fprintf(stderr, "Error on opcode %lu (%c), matching ", i, op);
	print_symbol(stderr, err.left);
	fprintf(stderr, " against ");
	print_symbol(stderr, err.right);
	fprintf(stderr, ", while unifying\n\t");
	TypePtrU64Map vars = {};
	print_type(stderr, left, &vars);
	fprintf(stderr, "\nagainst\n\t");
	print_type(stderr, right, &vars);
	map_free(&vars);
	fputc('\n', stderr);
}

internal
void add(Type *v, Type *t, TypePtrArray *var_stack) {
	Type *t0 = v->terms;
	if (t0 == NULL) {
		v->terms = t;
	} else {
		if (t0->next == t0) {
			array_push(var_stack, v);
		}
		Type *tmp = t0->next;
		t0->next = t->next;
		t->next = tmp;
	}
}

internal
void merge(Type *v1, Type *v2, TypePtrArray *var_stack) {
	u64 r1 = v1->var_count &~ VAR_BIT;
	u64 r2 = v2->var_count &~ VAR_BIT;
	Type *bigV, *v;
	if (r1 >= r2) {
		bigV = v1;
		v = v2;
	} else {
		bigV = v2;
		v = v1;
	}
	Type *t0 = v->terms;
	Type *t1 = bigV->terms;
	// This is checking if bigV has at most 1 terms and the sum of the
	// number of terms is at least two
	if ((t1 == NULL && t0 != NULL && t0->next != t0) || (t1 != NULL && t1->next == t1 && t0 != NULL)) {
		array_push(var_stack, bigV);
	}
	if (t1 == NULL) {
		bigV->terms = t0;
	} else if (t0 != NULL) {
		Type *tmp = t0->next;
		t0->next = t1->next;
		t1->next = tmp;
	}
	v->rep = bigV;
	v->terms = NULL;
	v->var_count = VAR_BIT;
	bigV->var_count = r1 + r2 | VAR_BIT;
}

internal
UnificationError commonFrontier(TypePtrArray t_list, TypePtrArray *var_stack) {
	// TODO benchmark with and without checks for identical nodes
	u64 sym = t_list.data[0]->symbol;
	if (IS_SEALED(sym)) {
		foreach (term, t_list) {
			if ((*term)->symbol != sym) {
				return (UnificationError){
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
				return (UnificationError){
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
	TypePtrArray t0_list;
	t0_list.cap = t0_list.size = t_list.size;
	t0_list.data = alloca(t0_list.cap * sizeof(Type *));
	// TODO eliminate these stacks
	usize *s0_backing = alloca(t_list.size * sizeof(usize));
	usize *s1_backing = alloca(t_list.size * sizeof(usize));
	for (usize i = 0; i < a; i++) {
		for (usize j = 0; j < t_list.size; j++) {
			t0_list.data[j] = (&t_list.data[j]->child1)[i];
		}
		USizeArray s0 = {};
		USizeArray s1 = {};
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
			Type *v = rep(t0_list.data[j]);
			foreach (k, s0) {
				Type *v2 = rep(t0_list.data[*k]);
				if (v != v2) {
					merge(v, v2, var_stack);
				}
			}
			foreach (k, s1) {
				add(v, t0_list.data[*k], var_stack);
			}
		} else {
			UnificationError err = commonFrontier(t0_list, var_stack);
			if (err.left != err.right) {
				return err;
			}
		}
	}
	return (UnificationError){};
}

// This uses a slightly modified version of Joxan Jaffar's efficient infinite
// unification algorithm.
internal
UnificationError unify(Type *left, Type *right) {
	TypePtrArray var_stack = {};
	b32 left_term = !IS_VAR(left);
	b32 right_term = !IS_VAR(right);
	UnificationError err = {};
	if (left_term && right_term) {
		// TODO benchmark with and without
		if (left == right) {
			return (UnificationError){};
		}
		Type *terms[] = {left, right};
		TypePtrArray t_list = {terms, 2, 2};
		err = commonFrontier(t_list, &var_stack);
	} else if (left_term) {
		Type* right_rep = rep(right);
		add(right_rep, left, &var_stack);
	} else if (right_term) {
		Type* left_rep = rep(left);
		add(left_rep, right, &var_stack);
	} else {
		Type* left_rep = rep(left);
		Type* right_rep = rep(right);
		if (left_rep != right_rep) {
			merge(left_rep, right_rep, &var_stack);
		} else {
			return (UnificationError){};
		}
	}
	if (err.left != err.right) {
		array_free(&var_stack);
		return err;
	}
	while (var_stack.size > 0) {
		Type *v = array_pop(&var_stack);
		u64 k = 0;
		if (v->terms != NULL) {
			k = 1;
			Type *term = v->terms;
			while (term->next != v->terms) {
				k++;
				term = term->next;
			}
		}
		if (k >= 2) {
			Type *t_data[k];
			TypePtrArray t;
			t.cap = t.size = k;
			t.data = t_data;
			Type *t0 = v->terms;
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
	return (UnificationError){};
}

internal
void assign(Type *var, Type *term) {
	assert(var->terms == NULL);
	assert(var->rep == var);
	var->terms = term;
}

internal
void assign_term(Type *var, u64 symbol, Type *c1, Type *c2, TypePool *pool) {
	Type *term = alloc_type(pool);
	set_term(term, symbol, c1, c2);
	assign(var, term);
}

internal
void assign_sealed(Type *var, StringRC *seal, Type *c1, TypePool *pool) {
	Type *term = alloc_type(pool);
	set_sealed(term, seal, c1);
	assign(var, term);
}

#if 0
b32 expect_(u8 *pat, Type **input, Type **vars, TypePool *pool) {
	Type **locs[8];
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

internal
void print_expect_error(usize i, u8 op, Type* loc, Type *input) {
	fprintf(stderr, "Error on opcode %lu (%c), expected product, got ", i, op);
	print_symbol(stderr, loc->symbol);
	fputc('\n', stderr);
	print_type_single(stderr, input);
	fputc('\n', stderr);
}

// TODO Type inference can happen *after* graph construction and the most
// beneficial optimisations
// TODO clean up error reporting
internal
b32 infer_block(Block *block, TypePool *pool) {

#define prod( c1, c2) set_term(alloc_type(pool), SYMBOL_PRODUCT, c1, c2)
#define sum(  c1, c2) set_term(alloc_type(pool), SYMBOL_SUM,     c1, c2)
#define block(c1, c2) set_term(alloc_type(pool), SYMBOL_BLOCK,   c1, c2)
#define sealed(seal, ty) set_sealed(alloc_type(pool), seal, ty)

	block->types = malloc((block->size + 1) * sizeof(Type *));
	block->types[0] = var();
	AOStackFrame *frame = NULL;
	for (usize i = 0, frame_index = 0, block_index = 0, sealer_index = 0; i < block->size; i++) {
		u8 op = block->opcodes[i];
		Type *input = block->types[i];
		Type *output;
		Type *vars[5];
#if 0
#define expect(pat) if (!expect_((u8 *) (pat), &input, vars, pool)) { \
	fprintf(stderr, "Error on opcode %lu (%c)\n", i, op); \
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
					Block *b = block->blocks[block_index++];
					Type *bty = block(b->types[0], b->types[b->size]);
					bty->symbol |= POLYMORPHIC_BIT;
					output(prod(inst(bty, pool), input));
				}
			case '"':     output(prod(pool->text, input));
			case OP_SEAL: //optype: *vv prod(sealed(block->sealers[sealer_index++], vars[0]), vars[1])
			case OP_UNSEAL:
				{
					StringRC *seal = block->sealers[sealer_index++];
					//expect: *vv
					Type *type = deref(vars[0]);
					if (!IS_VAR(type)) {
						if (IS_SEALED(type->symbol)) {
							if (type->seal != seal) {
								fprintf(stderr, "Error on opcode %lu (%c), attempted to unseal value sealed with \"", i, op);
								print_string(stderr, type->seal);
								fprintf(stderr, "\" using unsealer \"");
								print_string(stderr, seal);
								fprintf(stderr, "\"\n");
								fail();
							}
						} else {
							fprintf(stderr, "Error on opcode %lu, unsealing non-sealed value ", i);
							print_symbol(stderr, type->symbol);
							fprintf(stderr, " with sealer \"");
							print_string(stderr, seal);
							fprintf(stderr, "\"\n");
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
					for (Type *tortoise = deref(input);
					     vars[0] != pool->text && vars[0] != tortoise;
					     tortoise = deref(deref(deref(tortoise)->child1)->child2),
					     input = vars[0]) {
						//expect: +*N+*Nv11 // Two iterations of the loop
						vars[0] = deref(vars[0]);
						if (IS_VAR(vars[0])) {
							assign(vars[0], pool->text->terms);
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
					Type *t = vars[0];
					if (!IS_VAR(t)) {
						t = var();
						t->terms = vars[0];
					}
					output(prod(t, prod(inst(t, pool), vars[1])));
				}

			case '$': // TODO incorporate fixpoint stuff for semi-polymorphic recursion
				{
					//expect: *[vv*vv
					Type *input_term = deref(input);
					Type *b = inst(deref(input_term->child1), pool);
					UnificationError err = unify(b->child1, inst(vars[2], pool));
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
					Type *input_term = deref(input);
					Type *child1_term = deref(input_term->child1);
					Type *child21_term = deref(deref(input_term->child2)->child1);
					Type *b1 = inst(child1_term, pool);
					Type *b2 = inst(child21_term, pool);
					UnificationError err = unify(b1->child2, b2->child1);
					if (err.left != err.right) {
						Type *left = child1_term->child2;
						Type *right = child21_term->child1;
						print_unification_error(i, op, err, left, right);
						fail();
					}
					Type *result = block(b1->child1, b2->child2);
					output(prod(result, vars[4]));
				}
			case '\'':
				{
					// TODO make this polymorphic iff the value is constant
					//expect: *vv
					Type *s = var();
					(void) s;
					Type *c = deref(vars[0]);
					Type *b = block(s, prod(c, s));
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
					Type *child1_term = deref(deref(input)->child1);
					Type *b = inst(child1_term, pool);
					UnificationError err = unify(b->child1, inst(vars[2], pool));
					if (err.left != err.right) {
						print_unification_error(i, op, err, child1_term->child1, vars[2]);
						fail();
					}
					output(prod(sum(b->child2, vars[3]), vars[4]));
				}
			case 'D':
				{
					//expect: *v*+vvv
					Type *t = vars[0];
					if (!IS_VAR(t)) {
						t = var();
						t->terms = vars[0];
					}
					output(prod(sum(prod(t, vars[1]), prod(inst(t, pool), vars[2])), vars[3]));
				}
			case 'F': //optype: *+*vv*vvv prod(sum(vars[0], vars[2]), prod(sum(vars[1], vars[3]), vars[4]))
			case 'M':
				{
					//expect: *+vvv
					Type *a = inst(vars[0], pool);
					UnificationError err = unify(a, inst(vars[1], pool));
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
					Type *num_pair = prod(pool->number, pool->number);
					output(prod(sum(num_pair, num_pair), vars[0]));
				}

			default:
			          assert(!"Received invalid opcode");
			          return false;
		}
		block->types[i+1] = output;
#undef output
#undef fail
#undef fail_expect
	}
	return true;
#undef prod
#undef sum
#undef block
#undef sealed
}

internal
b32 infer_types(BlockPtrArray blocks, TypePool *pool) {
	pool->unit   = set_term(alloc_type(pool), SYMBOL_UNIT,   NULL, NULL);
	pool->number = set_term(alloc_type(pool), SYMBOL_NUMBER, NULL, NULL);
	pool->text   = var();
	assign(pool->text, set_term(alloc_type(pool), SYMBOL_SUM,
	                            set_term(alloc_type(pool), SYMBOL_PRODUCT, pool->number, pool->text),
	                            pool->unit));
	pool->boolean = set_term(alloc_type(pool), SYMBOL_BOOL, NULL, NULL);
	foreach (block, blocks) {
		if (!infer_block(*block, pool)) {
			fprintf(stderr, "Failed in block %lu\n", block_index);
			return false;
		}
	}
	return true;
}
