#include "build_graphs.h"

#include "type.h"

#include <stdlib.h>

#define CHUNK_SIZE 4096

// TODO add a free list
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
	result->out_count = 1;
	return (struct link){result, 0, type};
}

internal inline
struct node *append_node10(struct node_pool *pool, u8 uop, struct link link) {
	struct node *result = alloc_node(pool, uop);
	result->in[0] = link.node;
	result->src_slot[0] = link.slot;
	link.node->out[link.slot] = result;
	link.node->dst_slot[link.slot] = 0;
	result->in_count = 1;
	return result;
}

internal inline
struct link append_node11(struct node_pool *pool, u8 uop, struct link link, union type *type) {
	struct node *result = append_node10(pool, uop, link);
	result->output_type[0] = type;
	result->out_count = 1;
	return (struct link){result, 0, type};
}

internal inline
struct links append_node12(struct node_pool *pool, u8 uop, struct link link, union type *type1, union type *type2,
                           b32 optimise) {
	if (optimise) {
		if ((uop == UOP_UNPAIR && link.node->uop == UOP_PAIR) ||
		    (uop == UOP_UNSUM  && link.node->uop == UOP_SUM)) {
			return (struct links){
				{link.node->in[0], link.node->src_slot[0], type1},
				{link.node->in[1], link.node->src_slot[1], type2},
			};
		}
	}
	struct node *result = append_node10(pool, uop, link);
	result->output_type[0] = type1;
	result->output_type[1] = type2;
	result->out_count = 2;
	return (struct links){{result, 0, type1}, {result, 1, type2}};
}

internal inline
struct node *append_node20(struct node_pool *pool, u8 uop, struct link link1, struct link link2) {
	struct node *result = alloc_node(pool, uop);
	result->in[0] = link1.node;
	result->in[1] = link2.node;
	result->src_slot[0] = link1.slot;
	result->src_slot[1] = link2.slot;
	link1.node->out[link1.slot] = result;
	link2.node->out[link2.slot] = result;
	link1.node->dst_slot[link1.slot] = 0;
	link2.node->dst_slot[link2.slot] = 1;
	result->in_count = 2;
	return result;
}

internal inline
struct link append_node21(struct node_pool *pool, u8 uop,
                          struct link link1, struct link link2,
                          union type *type) {
	struct node *result = append_node20(pool, uop, link1, link2);
	result->output_type[0] = type;
	result->out_count = 1;
	return (struct link){result, 0, type};
}

internal inline
struct links append_node22(struct node_pool *pool, u8 uop,
			   struct link link1, struct link link2,
                           union type *type1, union type *type2) {
	struct node *result = append_node20(pool, uop, link1, link2);
	result->output_type[0] = type1;
	result->output_type[1] = type2;
	result->out_count = 2;
	return (struct links){{result, 0, type1}, {result, 1, type2}};
}

#define assert_product(type) assert(!IS_VAR(type) && (type)->symbol == SYMBOL_PRODUCT)
#define assert_sum(type)     assert(!IS_VAR(type) && (type)->symbol == SYMBOL_SUM)
#define child1(type) deref((type)->child1)
#define child2(type) deref((type)->child2)

internal inline
struct links unpair_(struct node_pool *pool, struct link link, b32 optimise) {
	assert_product(link.type);
	return append_node12(pool, UOP_UNPAIR, link, child1(link.type), child2(link.type), optimise);
}

internal inline
void unpair2_(struct node_pool *pool, struct link *links, struct link link, b32 optimise) {
	struct links outer = unpair_(pool, link, optimise);
	struct links inner = unpair_(pool, outer.right, optimise);
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
struct links unsum_(struct node_pool *pool, struct link link, b32 optimise) {
	assert_sum(link.type);
	return append_node12(pool, UOP_UNSUM, link, child1(link.type), child2(link.type), optimise);
}

internal inline
void unsum2_(struct node_pool *pool, struct link *links, struct link link, b32 optimise) {
	struct links outer = unsum_(pool, link, optimise);
	struct links inner = unsum_(pool, outer.right, optimise);
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
void build_graph(struct block *block, b32 optimise) {
	if (block->size == 0) {
		return;
	}
	struct node_pool pool = {};
	struct link links[4];
#define append01(uop, type) append_node01(&pool, (uop), &block->graph, (type))
#define append10(uop, link) append_node10(&pool, (uop), (link))
#define append11(uop, link, type) append_node11(&pool, (uop), (link), (type))
#define append12(uop, link, type1, type2) append_node12(&pool, (uop), (link), (type1), (type2), optimise)
#define append21(uop, link1, link2, type) \
	append_node21(&pool, (uop), (link1), (link2), (type))
#define append22(uop, link1, link2, type1, type2) \
	append_node22(&pool, (uop), (link1), (link2), (type1), (type2))
#define unpair(link) unpair_(&pool, (link), optimise)
#define unpair2(link) unpair2_(&pool, links, (link), optimise)
#define pair(link1, link2, type) append21(UOP_PAIR, (link1), (link2), (type))
#define pair2(link1, link2, link3, type) \
	pair2_(&pool, (link1), (link2), (link3), (type))
#define unsum(link) unsum_(&pool, (link), optimise)
#define unsum2(link) unsum2_(&pool, links, (link), optimise)
#define sum(link1, link2, type) append21(UOP_SUM, (link1), (link2), (type))
#define sum2(link1, link2, link3, type) \
	sum2_(&pool, (link1), (link2), (link3), (type))
	struct node fake_first = {};
	fake_first.uop = (enum uop)-1;
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
					block_constant.node->block = &block->blocks[block_index++]->graph;
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
					last = pair(zero_constant, last, output_type);
					break;
				}
			case '0'...'9':
				{
					u32 digit = op - '0';
					struct links input = unpair(last);
					if (input.left.node->uop == UOP_NUMBER_CONSTANT) {
						input.left.node->number = input.left.node->number * 10 + digit;
						break;
					}
					assert_product(output_type);
					union type *number = child1(output_type);
					struct link ten_constant = append01(UOP_NUMBER_CONSTANT, number);
					ten_constant.node->number = 10;
					struct link digit_constant = append01(UOP_NUMBER_CONSTANT, number);
					digit_constant.node->number = digit;
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
					struct link g0 = {greater, 0, links[1].type};
					struct link g1 = {greater, 1, links[0].type};
					struct link g2 = {greater, 2, links[0].type};
					struct link g3 = {greater, 3, links[1].type};
					greater->output_type[0] = g0.type;
					greater->output_type[1] = g1.type;
					greater->output_type[2] = g2.type;
					greater->output_type[3] = g3.type;
					greater->out_count = 4;
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
	fake_first.out[0]->in[fake_first.dst_slot[0]] = NULL;
	block->graph.input = fake_first.out[0];
	block->graph.input_slot = fake_first.dst_slot[0];
	block->graph.output = last.node;
	block->graph.output_slot = last.slot;
}

internal u64 global_traversal = 1;

internal char *uop_names[] = {
	"seal",
	"unseal",
	"create pair",
	"destroy pair",
	"create sum",
	"destroy sum",
	"unit constant",
	"void constant",
	"block constant",
	"number constant",
	"text constant",
	"copy",
	"drop",
	"apply",
	"compose",
	"quote",
	"mark relevant",
	"mark affine",
	"add",
	"multiply",
	"inverse",
	"negate",
	"divmod",
	"distribute",
	"merge",
	"greater",
	"assert copyable",
	"assert droppable",
	"assert nonzero",
	"assert void",
	"assert equal",
	"debug print raw",
	"debug print text",
};

#include <stdio.h>

char *in_port(u32 slot_count, u32 slot) {
	static char *ports[] = {"nw", "ne"};
	if (slot_count == 1) {
		return "c";
	} else {
		return ports[slot];
	}
}

char *out_port(u32 slot_count, u32 slot) {
	static char *ports[] = {"w", "sw", "s", "se", "e"};
	switch (slot_count) {
		case 1:
			return "c";
		case 2:
		        return ports[slot * 2 + 1];
		case 3:
		        return ports[slot + 1];
		case 4:
		        return ports[slot + slot / 2];
		default:
		        assert(!"Impossible out count");
	}
}

void print_node_link(struct node *from, u32 slot_count, u32 slot, struct graph *graph) {
	struct node *to = from->out[slot];
	printf("node_%p:%s -> node_", from, out_port(slot_count, slot));
	if (to == NULL) {
		printf("end_%p\n", graph);
	} else {
		printf("%p:%s\n", to, in_port(to->in_count, from->dst_slot[slot]));
	}
}

void print_node(struct node *node, struct graph *graph, u64 traversal) {
	if (node->seen == traversal) {
		return;
	}
	node->seen = traversal;
	switch (node->uop) {
		case UOP_NUMBER_CONSTANT:
			printf("node_%p [label=\"constant: %f\"]\n", node, node->number);
			break;
		case UOP_BLOCK_CONSTANT:
			if (node->block->input) {
				printf("node_%p [label=\"block constant\"]\n", node);
				printf("node_%p -> node_%p [style=dotted,constraint=false]\n",
				       node, node->block->input);
			} else {
				printf("node_%p [label=\"empty block constant\"]\n", node);
			}
			break;
		default:
			printf("node_%p [label=\"%s\"]\n", node, uop_names[node->uop]);
	}
	for (u32 i = 0; i < node->out_count; i++) {
		print_node_link(node, node->out_count, i, graph);
		if (node->out[i]) {
			print_node(node->out[i], graph, traversal);
		}
	}
}

void print_graph(struct graph *graph, b32 is_main, u64 traversal) {
	if (!is_main) {
		printf("subgraph cluster_%p {\n", graph);
	}
	printf("node_start_%p [label=\"START\"]\n", graph);
	printf("node_start_%p -> node_%p:%s\n",
	       graph, graph->input, in_port(graph->input->in_count, graph->input_slot));
	printf("node_end_%p [label=\"END\"]\n", graph);
	print_node(graph->input, graph, traversal);
	for (struct node *node = graph->constants; node; node = node->next_constant) {
		print_node(node, graph, traversal);
	}
	if (!is_main) {
		printf("}\n");
	}
}

void build_graphs(struct block_ptr_array blocks) {
	foreach (block, blocks) {
		build_graph(*block, false);
	}
	printf("digraph {\n");
	u64 traversal = global_traversal++;
	foreach (block, blocks) {
		print_graph(&(*block)->graph, block_index == blocks.size - 1, traversal);
	}
	printf("}\n");
}
