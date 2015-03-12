#include <stdlib.h>

#include "infer_types.h"
#include "map.h"

u32 type_hash(union type *key) {
	return (u32) ((u64)key / sizeof(union type));
}

DEFINE_MAP(union type *, union type *, type_ptr, type_hash);

#define CHUNK_SIZE 4096
struct types {
	struct type_ptr_array chunks;
	usize used;
	union type *unit;
	union type *number;
	union type *text;
};

union type *alloc_type(struct types *types) {
	if (types->used == CHUNK_SIZE || types->chunks.size == 0) {
		array_push(&types->chunks, malloc(CHUNK_SIZE * sizeof(union type)));
		types->used = 0;
	}
	return &types->chunks.data[types->chunks.size - 1][types->used++];
}

union type *set_term(union type *type, u64 symbol, union type *c1, union type *c2) {
	type->symbol = symbol;
	type->child1 = c1;
	type->child2 = c2;
	return type;
}

union type *set_sealed(union type *type, struct string_rc *seal, union type *ty) {
	type->seal = seal;
	type->child1 = ty;
	return type;
}

#define var() set_var(alloc_type(types))
union type *set_var(union type *type) {
	type->term_count = VAR_BIT;
	return type;
}

// TODO Use modified version of Tarjan's SCC algorithm as per
// http://stackoverflow.com/questions/28924321/copying-part-of-a-graph
union type *inst_copy(union type *type, b32 share_vars, struct type_ptr_map *copied, struct types *types) {
	struct type_ptr_map_get_result result = type_ptr_map_get(copied, type);
	if (result.value) {
		return result.value;
	}
	if ((IS_VAR(type) && share_vars)  ||
	    type->symbol == SYMBOL_UNIT   ||
	    type->symbol == SYMBOL_NUMBER ||
	    type == types->text) {
		if (IS_VAR(type)) {
			type->rep = type;
		}
		type_ptr_map_put_bucket(copied, type, type, result.bucket);
		return type;
	}
	union type *new = alloc_type(types);
	type_ptr_map_put_bucket(copied, type, new, result.bucket);
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
	new->child1 = inst_copy(type->child1, share_vars, copied, types);
	if (IS_SEALED(type->symbol)) {
		return new;
	}
	new->child2 = inst_copy(type->child2, share_vars, copied, types);
	return new;
}

union type *inst(union type *type, struct types *types) {
	struct type_ptr_map seen = {};
	union type *result = inst_copy(type, true, &seen, types);
	map_free(&seen);
	return result;
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

b32 infer_block(struct block *block, struct types *types) {

#define prod( c1, c2) set_term(alloc_type(types), SYMBOL_PRODUCT, c1, c2)
#define sum(  c1, c2) set_term(alloc_type(types), SYMBOL_SUM,     c1, c2)
#define block(c1, c2) set_term(alloc_type(types), SYMBOL_BLOCK,   c1, c2)
#define sealed(seal, ty) set_sealed(alloc_type(types), seal, ty)

	block->types = malloc((block->size + 1) * sizeof(union type *));
	block->types[0] = var();
	for (usize i = 0, block_index = 0, sealer_index = 0; i < block->size; i++) {
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
		switch (op) {
			// Identity opcodes
			case OP_FRAME_PUSH:
			case OP_FRAME_POP:
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
						if (vars[0]->seal != seal) {
							return false; // TODO error
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
					// XXX Fully typed types are self contained (but not necessarily vice versa)
					// Perhaps call them known types, since they may have variables?
					// inst must *only* copy polymorphic parts of the type

					//expect: *[vv*vv
					//union type *b = inst(input->child1);
					//unify(b->child1, inst(vars[2]));
					//struct type_ptr_map seen;
					//remove_vars(b->child2, &seen);
					//remove_vars(vars[3], &seen);
					//map_free(&seen);
					//output(prod(b->child2, vars[3]));
					assert(false);
					return false;
				}
			case 'o':
				{
					//expect: *[vv*[vvv
					//union type *b1 = inst(input->child1);
					//union type *b2 = inst(input->child2->child1);
					//unify(b1->child2, b2->child1);
					//struct type_ptr_map seen;
					//remove_vars(b1->child1, &seen);
					//remove_vars(b2->child2, &seen);
					//remove_vars(vars[4], &seen);
					//map_free(&seen);
					//union type *result = block(b1->child1, b2->child2);
					//output(prod(result, vars[4]));
					assert(false);
					return false;
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
					//union type *b = inst(input->child1);
					//unify(b->child1, inst(vars[2]));
					//struct type_ptr_map seen;
					//remove_vars(b->child2, &seen);
					//remove_vars(vars[3], &seen);
					//remove_vars(vars[4], &seen);
					//map_free(&seen);
					//output(prod(sum(b->child2, vars[3]), vars[4]));
					assert(false);
					return false;
				}
			case 'D': //optype: *v*+vvv prod(sum(prod(vars[0], vars[1]), prod(vars[0], vars[2])), vars[3])
			case 'F': //optype: *+*vv*vvv prod(sum(vars[0], vars[2]), prod(sum(vars[1], vars[3]), vars[4]))
			case 'M':
				{
					//expect: *+vvv
					//union type *a = inst(vars[0]);
					//unify(a, inst(vars[1]));
					//struct type_ptr_map seen;
					//remove_vars(a, &seen);
					//remove_vars(vars[2], &seen);
					//map_free(&seen);
					//output(prod(a, vars[2]));
					assert(false);
					return false;
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
