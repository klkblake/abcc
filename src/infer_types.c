#include <stdlib.h>

#include "infer_types.h"

#define CHUNK_SIZE 4096
struct types {
	struct type_ptr_slice chunks;
	usize used;
	union type *unit;
	union type *number;
	union type *text;
};

union type *alloc_node(struct types *types) {
	if (types->used == CHUNK_SIZE) {
		slice_snoc(&types->chunks, malloc(CHUNK_SIZE * sizeof(union type)));
		types->used = 0;
	}
	return &types->chunks.data[types->chunks.size - 1][types->used++];
}

union type *alloc_type(u64 symbol, union type *c1, union type *c2, struct types *types) {
	union type *node = alloc_node(types);
	node->symbol = symbol;
	node->next = node;
	node->child1 = c1;
	node->child2 = c2;
	return node;
}

union type *alloc_var_node(struct types *types) {
	union type *node = alloc_node(types);
	*node = (union type){};
	node->rep = node;
	node->var_count = VAR_COUNT_BIT | 1;
	return node;
}

struct opcode_type_result {
	union type *input;
	union type *output;
};

//struct opcode_type_result opcode_type(block);

void infer_block(struct block *block, struct types *types) {
	block->types = malloc((block->size + 1) * sizeof(union type *));
	if (block->size == 0) {
		block->types[0] = alloc_var_node(types);
		return;
	}
	for (usize i = 0, block_index = 0, sealer_index = 0; i < block->size; i++) {
		u8 op = block->opcodes[i];
		union type *to_unify;
#define varx() alloc_var_node(types)
#define var(name) union type *name = varx()
#define var2(a, b)       var(a); var(b)
#define var3(a, b, c)    var(a); var(b); var(c)
#define var4(a, b, c, d) var(a); var(b); var(c); var(d)
#define term2(type, c1, c2) alloc_type(SYMBOL_ ## type, c1, c2, types)
#define term1(type, c1) term2(type, c1, NULL)
#define prod(c1, c2) term2(PRODUCT, c1, c2)
#define optype(left, right) to_unify = (left); block->types[i+1] = (right); break
#define optype_(stmts, left, right) do { stmts; optype(left, right); } while (false)
#define optype1(a,          left, right) optype_(var(a),           left, right)
#define optype2(a, b,       left, right) optype_(var2(a, b),       left, right)
#define optype3(a, b, c,    left, right) optype_(var3(a, b, c),    left, right)
#define optype4(a, b, c, d, left, right) optype_(var4(a, b, c, d), left, right)
		switch (op) {
			// Identity opcodes
			case OP_FRAME_PUSH:
			case OP_FRAME_POP:
			case OP_DEBUG_PRINT_RAW:
				{
					if (i == 0) {
						block->types[0] = varx();
					}
					block->types[i+1] = block->types[i];
					continue;
				}

			// Opcodes that need additional state access
			case '[':
				{
					// Blocks are topologically ordered, so we know we have
					// already processed this one
					struct block *b = block->blocks[block_index++];
					block->types[i+1] = prod(term2(BLOCK, b->types[0], b->types[b->size]),
					                         block->types[i]);
					continue;
				}
			case '"':
				{
					block->types[i+1] = prod(types->text, block->types[i]);
					continue;
				}
			case OP_SEAL:
				{
					var2(a, e);
					union type *sealed = term1(VOID /* overriden by next line */, a);
					sealed->seal = block->sealers[sealer_index++];
					optype(prod(a, e), prod(sealed, e));
				}
			case OP_UNSEAL:
				{
					var2(a, e);
					union type *sealed = term1(VOID /* overriden by next line */, a);
					sealed->seal = block->sealers[sealer_index++];
					optype(prod(sealed, e), prod(a, e));
				}

			// Normal opcodes
			case OP_ASSERT_EQUAL:
				{
					var2(a, e);
					union type *ty = prod(a, prod(a, e));
					optype(ty, ty);
				}
			case OP_DEBUG_PRINT_TEXT:
				optype(types->text, types->text);

			case 'l': optype3(a, b, c, prod(a, prod(b, c)), prod(prod(a, b), c));
			case 'r': optype3(a, b, c, prod(prod(a, b), c), prod(a, prod(b, c)));
			case 'w': optype3(a, b, c, prod(a, prod(b, c)), prod(b, prod(a, c)));
			case 'z': optype4(a, b, c, d, prod(a, prod(b, prod(c, d))), prod(a, prod(c, prod(b, d))));
			case 'v':
				{
					if (i == 0) {
						block->types[0] = varx();
					}
					block->types[i+1] = prod(block->types[i], types->unit);
					continue;
				}
			case 'c': optype1(a, prod(a, types->unit), a);
			case '%': optype2(x, e, prod(x, e), e);
			case '^': optype2(x, e, prod(x, e), prod(x, prod(x, e)));

			case '$': // TODO figure out what the hell to do here
			case 'o':
			case '\'':
			          assert(false);
			case 'k':
			case 'f':
				  {
					  var2(b, e);
					  union type *ty = prod(b, e);
					  optype(ty, ty);
				  }

			case '#':
				{
					if (i == 0) {
						block->types[0] = varx();
					}
					block->types[i+1] = prod(types->number, block->types[i]);
					continue;
				}
			case '0'...'9':
				  {
					  var(e);
					  union type *ty = prod(types->number, e);
					  optype(ty, ty);
				  }

			case '+':
			case '*':
				  {
					  var(e);
					  union type *ty = prod(types->number, e);
					  optype(prod(types->number, ty), ty);
				  }

			default: assert(false);
		}
		if (i == 0) {
			block->types[0] = to_unify;
			continue;
		}
		// TODO perform unification
	}
}

void infer_types(struct block_ptr_slice blocks) {
	struct types types = {};
	types.unit   = alloc_type(SYMBOL_UNIT,   NULL, NULL, &types);
	types.number = alloc_type(SYMBOL_NUMBER, NULL, NULL, &types);
	types.text = alloc_type(SYMBOL_SUM,
	                             alloc_type(SYMBOL_PRODUCT, types.number, NULL, &types),
				     types.unit,
				     &types);
	types.text->child1->child2 = types.text;
	foreach (block, blocks) {
		infer_block(*block, &types);
	}
}
