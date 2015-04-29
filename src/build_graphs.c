#include "build_graphs.h"

#include "type.h"

#include <stdlib.h>

#define CHUNK_SIZE 4096

struct node_pool {
	struct node_ptr_array chunks;
	usize used;
};

internal
struct node *alloc_node(struct node_pool *pool, enum uop uop) {
	if (pool->used == CHUNK_SIZE || pool->chunks.size == 0) {
		array_push(&pool->chunks, malloc(CHUNK_SIZE * sizeof(struct node)));
		pool->used = 0;
	}
	struct node *result = &pool->chunks.data[pool->chunks.size - 1][pool->used++];
	*result = (struct node){};
	result->uop = uop;
	return result;
}

struct link {
	struct node *node;
	u32 slot;
	union type *type;
};

struct links {
	struct link left;
	struct link right;
};

internal inline
struct link append_node01(struct node_pool *pool, u8 uop, struct graph *graph, union type *type) {
	struct node *result = alloc_node(pool, uop);
	result->next_constant = graph->constants;
	graph->constants = result;
	result->output_type[0] = type;
	return (struct link){result, 0, type};
}

internal inline
struct node *append_node10(struct node_pool *pool, u8 uop, struct link link) {
	struct node *result = alloc_node(pool, uop);
	result->in[0] = link.node;
	link.node->out[link.slot] = result;
	return result;
}

internal inline
struct link append_node11(struct node_pool *pool, u8 uop, struct link link, union type *type) {
	struct node *result = append_node10(pool, uop, link);
	result->output_type[0] = type;
	return (struct link){result, 0, type};
}

internal inline
struct links append_node12(struct node_pool *pool, u8 uop, struct link link, union type *type1, union type *type2) {
	struct node *result = append_node10(pool, uop, link);
	result->output_type[0] = type1;
	result->output_type[1] = type2;
	return (struct links){{result, 0, type1}, {result, 1, type2}};
}

internal inline
struct node *append_node20(struct node_pool *pool, u8 uop, struct link link1, struct link link2) {
	struct node *result = alloc_node(pool, uop);
	result->in[0] = link1.node;
	result->in[1] = link2.node;
	link1.node->out[link1.slot] = result;
	link2.node->out[link2.slot] = result;
	return result;
}

internal inline
struct link append_node21(struct node_pool *pool, u8 uop,
                          struct link link1, struct link link2,
                          union type *type) {
	struct node *result = append_node20(pool, uop, link1, link2);
	result->output_type[0] = type;
	return (struct link){result, 0, type};
}

internal inline
struct links append_node22(struct node_pool *pool, u8 uop,
			   struct link link1, struct link link2,
                           union type *type1, union type *type2) {
	struct node *result = append_node20(pool, uop, link1, link2);
	result->output_type[0] = type1;
	result->output_type[1] = type2;
	return (struct links){{result, 0, type1}, {result, 1, type2}};
}

#define assert_product(type) assert(!IS_VAR(type) && (type)->symbol == SYMBOL_PRODUCT)
#define assert_sum(type)     assert(!IS_VAR(type) && (type)->symbol == SYMBOL_SUM)
#define child1(type) deref((type)->child1)
#define child2(type) deref((type)->child2)

internal inline
struct links unpair_(struct node_pool *pool, struct link link) {
	assert_product(link.type);
	return append_node12(pool, UOP_UNPAIR, link, child1(link.type), child2(link.type));
}

internal inline
void unpair2_(struct node_pool *pool, struct link *links, struct link link) {
	struct links outer = unpair_(pool, link);
	struct links inner = unpair_(pool, outer.right);
	links[0] = outer.left;
	links[1] = inner.left;
	links[2] = inner.right;
}

internal inline
struct link pair2_(struct node_pool *pool,
                   struct link link1, struct link link2, struct link link3,
                   union type *type) {
	assert_product(type);
	struct link right = append_node21(pool, UOP_PAIR, link2, link3, child2(type));
	return append_node21(pool, UOP_PAIR, link1, right, type);
}

internal inline
struct links unsum_(struct node_pool *pool, struct link link) {
	assert_sum(link.type);
	return append_node12(pool, UOP_UNSUM, link, child1(link.type), child2(link.type));
}

internal inline
void unsum2_(struct node_pool *pool, struct link *links, struct link link) {
	struct links outer = unsum_(pool, link);
	struct links inner = unsum_(pool, outer.right);
	links[0] = outer.left;
	links[1] = inner.left;
	links[2] = inner.right;
}

internal inline
struct link sum2_(struct node_pool *pool,
                   struct link link1, struct link link2, struct link link3,
                   union type *type) {
	assert_sum(type);
	struct link right = append_node21(pool, UOP_SUM, link2, link3, child2(type));
	return append_node21(pool, UOP_SUM, link1, right, type);
}

internal
void build_graph(struct block *block) {
	if (block->size == 0) {
		return;
	}
	struct node_pool pool = {};
	struct link links[4];
#define append01(uop, type) append_node01(&pool, (uop), &block->graph, (type))
#define append10(uop, link) append_node10(&pool, (uop), (link))
#define append11(uop, link, type) append_node11(&pool, (uop), (link), (type))
#define append12(uop, link, type1, type2) append_node12(&pool, (uop), (link), (type1), (type2))
#define append21(uop, link1, link2, type) \
	append_node21(&pool, (uop), (link1), (link2), (type))
#define append22(uop, link1, link2, type1, type2) \
	append_node22(&pool, (uop), (link1), (link2), (type1), (type2))
#define unpair(link) unpair_(&pool, (link))
#define unpair2(link) unpair2_(&pool, links, (link))
#define pair(link1, link2, type) append21(UOP_PAIR, (link1), (link2), (type))
#define pair2(link1, link2, link3, type) \
	pair2_(&pool, (link1), (link2), (link3), (type))
#define unsum(link) unsum_(&pool, (link))
#define unsum2(link) unsum2_(&pool, links, (link))
#define sum(link1, link2, type) append21(UOP_SUM, (link1), (link2), (type))
#define sum2(link1, link2, link3, type) \
	sum2_(&pool, (link1), (link2), (link3), (type))
	struct node fake_first = {};
	fake_first.output_type[0] = deref(block->types[0]);
	struct link last = {&fake_first, 0, fake_first.output_type[0]};
	struct ao_stack_frame *frame = NULL;
	for (usize i = 0, frame_index = 0, block_index = 0, text_index = 0, sealer_index = 0; i < block->size; i++) {
		u8 op = block->opcodes[i];
		union type *output_type = deref(block->types[i + 1]);
		switch (op) {
			case OP_FRAME_PUSH: frame = block->frames[frame_index++]; break;
			case OP_FRAME_POP:  frame = frame->next; break;
			case OP_SEAL:
				{
					last = append11(UOP_SEAL, last, output_type);
					last.node->seal = block->sealers[sealer_index++];
					break;
				}
			case OP_UNSEAL:
				{
					last = append11(UOP_UNSEAL, last, output_type);
					last.node->seal = block->sealers[sealer_index++];
					break;
				}
			case OP_ASSERT_EQUAL:
				{
					unpair2(last);
					struct links assert_equal = append22(UOP_ASSERT_EQUAL,
					                                     links[0], links[1],
					                                     links[0].type, links[1].type);
					last = pair2(assert_equal.left, assert_equal.right, links[2], output_type);
					break;
				}
			case OP_DEBUG_PRINT_RAW:
				{
					last = append11(UOP_DEBUG_PRINT_RAW, last, output_type);
					break;
				}
			case OP_DEBUG_PRINT_TEXT:
				{
					last = append11(UOP_DEBUG_PRINT_TEXT, last, output_type);
					break;
				}
			case '[':
				{
					assert_product(output_type);
					struct link block_constant = append01(UOP_BLOCK_CONSTANT,
					                                      child1(output_type));
					block_constant.node->block = &block->blocks[block_index]->graph;
					last = pair(block_constant, last, output_type);
					break;
				}
			case '"':
				{
					assert_product(output_type);
					struct link text_constant = append01(UOP_TEXT_CONSTANT,
					                                     child1(output_type));
					text_constant.node->text = block->texts[text_index++];
					last = pair(text_constant, last, output_type);
					break;
				}

			case 'l':
				{
					unpair2(last);
					assert_product(output_type);
					struct link left = pair(links[0], links[1], child1(output_type));
					last = pair(left, links[2], output_type);
					break;
				}
			case 'r':
				{
					struct links outer = unpair(last);
					struct links inner = unpair(outer.left);
					last = pair2(inner.left, inner.right, outer.right, output_type);
					break;
				}
			case 'w':
				{
					unpair2(last);
					last = pair2(links[1], links[0], links[2], output_type);
					break;
				}
			case 'z':
				{
					unpair2(last);
					struct links inner = unpair(links[2]);
					assert_product(output_type);
					union type *type = child2(output_type);
					assert_product(type);
					links[2] = pair(links[1], inner.right, child2(type));
					last = pair2(links[0], inner.left, links[2], output_type);
					break;
				}
			case 'v':
				{
					assert_product(output_type);
					struct link unit_constant = append01(UOP_UNIT_CONSTANT,
					                                     child1(output_type));
					last = pair(last, unit_constant, output_type);
					break;
				}
			case 'c':
				{
					struct links input = unpair(last);
					append10(UOP_DROP, input.right);
					last = input.left;
					break;
				}
			case '%':
				{
					struct links input = unpair(last);
					struct link to_drop = append11(UOP_ASSERT_DROPPABLE,
					                               input.left, input.left.type);
					append10(UOP_DROP, to_drop);
					last = input.right;
					break;
				}
			case '^':
				{
					struct links input = unpair(last);
					struct link to_copy = append11(UOP_ASSERT_COPYABLE,
					                               input.left, input.left.type);
					struct links copy = append12(UOP_COPY, to_copy,
					                             to_copy.type, to_copy.type);
					last = pair2(copy.left, copy.right, input.right, output_type);
					break;
				}

			case '$':
				{
					unpair2(last);
					assert_product(output_type);
					struct link apply = append21(UOP_APPLY,
					                             links[0], links[1],
					                             child1(output_type));
					last = pair(apply, links[2], output_type);
					break;
				}
			case 'o':
				{
					unpair2(last);
					assert_product(output_type);
					struct link compose = append21(UOP_COMPOSE,
					                               links[0], links[1],
					                               child1(output_type));
					last = pair(compose, links[2], output_type);
					break;
				}
			case '\'':
				{
					struct links input = unpair(last);
					assert_product(output_type);
					struct link quote = append11(UOP_QUOTE, input.left, child1(output_type));
					last = pair(quote, input.right, output_type);
					break;
				}
			case 'k':
				{
					struct links input = unpair(last);
					assert_product(output_type);
					struct link relevant = append11(UOP_MARK_RELEVANT,
					                                input.left, child1(output_type));
					last = pair(relevant, input.right, output_type);
					break;
				}
			case 'f':
				{
					struct links input = unpair(last);
					assert_product(output_type);
					struct link affine = append11(UOP_MARK_AFFINE,
					                              input.left, child1(output_type));
					last = pair(affine, input.right, output_type);
					break;
				}

			case '#':
				{
					assert_product(output_type);
					struct link zero_constant = append01(UOP_NUMBER_CONSTANT,
					                                     child1(output_type));
					zero_constant.node->number = 0;
					last = pair(last, zero_constant, output_type);
					break;
				}
			case '0'...'9':
				{
					struct links input = unpair(last);
					assert_product(output_type);
					union type *number = child1(output_type);
					struct link ten_constant = append01(UOP_NUMBER_CONSTANT, number);
					ten_constant.node->number = 10;
					struct link digit_constant = append01(UOP_NUMBER_CONSTANT, number);
					digit_constant.node->number = op - '0';
					struct link mul_by_10 = append21(UOP_MULTIPLY,
					                                 input.left, ten_constant, number);
					struct link add_digit = append21(UOP_ADD,
					                                 mul_by_10, digit_constant, number);
					last = pair(add_digit, input.right, output_type);
					break;
				}

			case '+':
				{
					unpair2(last);
					assert_product(output_type);
					struct link add = append21(UOP_ADD, links[0], links[1], child1(output_type));
					last = pair(add, links[2], output_type);
					break;
				}
			case '*':
				{
					unpair2(last);
					assert_product(output_type);
					struct link multiply = append21(UOP_MULTIPLY,
					                                links[0], links[1], child1(output_type));
					last = pair(multiply, links[2], output_type);
					break;
				}
			case '/':
				{
					struct links input = unpair(last);
					assert_product(output_type);
					union type *number = child1(output_type);
					struct link checked_number = append11(UOP_ASSERT_NONZERO, input.left, number);
					struct link inverse = append11(UOP_INVERSE, checked_number, number);
					last = pair(inverse, input.right, output_type);
					break;
				}
			case '-':
				{
					struct links input = unpair(last);
					assert_product(output_type);
					struct link negate = append11(UOP_NEGATE, input.left, child1(output_type));
					last = pair(negate, input.right, output_type);
					break;
				}
			case 'Q':
				{
					unpair2(last);
					assert_product(output_type);
					union type *number = child1(output_type);
					struct link checked_number = append11(UOP_ASSERT_NONZERO, links[0], number);
					struct links divmod = append22(UOP_DIVMOD,
					                               checked_number, links[1],
					                               number, number);
					last = pair2(divmod.left, divmod.right, links[2], output_type);
					break;
				}

			case 'L':
				{
					struct links input = unpair(last);
					unsum2(input.left);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					struct link inner = sum(links[0], links[1], child1(type));
					struct link outer = sum(inner, links[2], type);
					last = pair(outer, input.right, output_type);
					break;
				}
			case 'R':
				{
					struct links input = unpair(last);
					struct links outer = unsum(input.left);
					struct links inner = unsum(outer.left);
					assert_product(output_type);
					struct link new_outer = sum2(inner.left, inner.right, outer.right,
					                             child1(output_type));
					last = pair(new_outer, input.right, output_type);
					break;
				}
			case 'W':
				{
					struct links input = unpair(last);
					unsum2(input.left);
					assert_product(output_type);
					struct link branches = sum2(links[1], links[0], links[2],
					                            child1(output_type));
					last = pair(branches, input.right, output_type);
					break;
				}
			case 'Z':
				{
					struct links input = unpair(last);
					unsum2(input.left);
					struct links inner = unsum(links[2]);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					union type *type2 = child2(type);
					assert_sum(type2);
					struct link new_inner = sum(links[1], inner.right, child2(type2));
					struct link branches = sum2(links[0], links[2], new_inner, type);
					last = pair(branches, input.right, output_type);
					break;
				}
			case 'V':
				{
					struct links input = unpair(last);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					struct link void_constant = append01(UOP_VOID_CONSTANT, child2(type));
					struct link branches = sum(input.left, void_constant, type);
					last = pair(branches, input.right, output_type);
					break;
				}
			case 'C':
				{
					struct links input = unpair(last);
					struct links branches = unsum(input.left);
					append10(UOP_DROP, branches.right);
					last = pair(branches.left, input.right, output_type);
					break;
				}

			case '?':
				{
					unpair2(last);
					struct link checked_block = append11(UOP_ASSERT_DROPPABLE,
					                                     links[0], links[0].type);
					struct links branches = unsum(links[1]);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					struct link applied_branch = append21(UOP_APPLY,
					                                      checked_block, branches.left,
					                                      child1(type));
					struct link new_branches = sum(applied_branch, branches.right,
					                               type);
					last = pair(new_branches, links[2], output_type);
					break;
				}
			case 'D':
				{
					unpair2(last);
					struct links branches = unsum(links[1]);
					struct links distrib = append12(UOP_DISTRIB, links[0],
					                                links[0].type, links[0].type);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					struct link left = pair(distrib.left, branches.left,
					                        child1(type));
					struct link right = pair(distrib.right, branches.right,
								 child2(type));
					struct link new_branches = sum(left, right, type);
					last = pair(new_branches, links[2], output_type);
					break;
				}
			case 'F':
				{
					struct links input = unpair(last);
					struct links branches = unsum(input.left);
					struct links left = unpair(branches.left);
					struct links right = unpair(branches.right);
					assert_product(output_type);
					union type *type = child2(output_type);
					assert_product(type);
					struct link new_left = sum(left.left, right.left,
					                           child1(output_type));
					struct link new_right = sum(left.right, right.right,
					                           child1(type));
					last = pair2(new_left, new_right, input.right, output_type);
					break;
				}
			case 'M':
				{
					struct links input = unpair(last);
					struct links branches = unsum(input.left);
					assert_product(output_type);
					struct link merge = append21(UOP_MERGE, branches.left, branches.right,
					                             child1(output_type));
					last = pair(merge, input.right, output_type);
					break;
				}
			case 'K':
				{
					struct links input = unpair(last);
					struct links branches = unsum(input.left);
					append10(UOP_ASSERT_VOID, branches.left);
					last = pair(branches.right, input.right, output_type);
					break;
				}

			case '>':
				{
					unpair2(last);
					struct node *greater = append_node20(&pool, UOP_GREATER, links[0], links[1]);
					union type *type1 = links[0].node->output_type[links[0].slot];
					union type *type2 = links[1].node->output_type[links[1].slot];
					struct link g0 = {greater, 0, type2};
					struct link g1 = {greater, 1, type1};
					struct link g2 = {greater, 2, type1};
					struct link g3 = {greater, 3, type2};
					greater->output_type[0] = g0.type;
					greater->output_type[1] = g1.type;
					greater->output_type[2] = g2.type;
					greater->output_type[3] = g3.type;
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					struct link left  = pair(g0, g1, child1(type));
					struct link right = pair(g2, g3, child2(type));
					struct link branches = append21(UOP_SUM, left, right, type);
					last = pair(branches, links[2], output_type);
					break;
				}

			default:
				{
					assert(!"Received invalid opcode");
				}
		}
	}
	block->graph.input = fake_first.out[0];
	if (block->graph.input->in[0] == &fake_first) {
		block->graph.input->in[0] = NULL;
	} else {
		block->graph.input->in[1] = NULL;
	}
}

b32 build_graphs(struct block_ptr_array blocks) {
	foreach (block, blocks) {
		build_graph(*block);
	}
	return true;
}
