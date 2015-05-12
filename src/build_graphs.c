#include "infer_types.gen.c"

internal
struct node *alloc_node(struct graph_pools *pools, enum uop uop) {
	struct node *result = alloc(&pools->node_pool, sizeof(struct node));
	result->uop = uop;
	return result;
}

internal
struct in_link *in_link(struct node *node, u32 index) {
	struct in_link_chunk *chunk = &node->in;
	while (index >= array_count(chunk->links)) {
		index -= array_count(chunk->links);
		chunk = chunk->next;
	}
	return &chunk->links[index];
}

internal
struct out_link *out_link(struct node *node, u32 index) {
	struct out_link_chunk *chunk = &node->out;
	while (index >= array_count(chunk->links)) {
		index -= array_count(chunk->links);
		chunk = chunk->next;
	}
	return &chunk->links[index];
}

internal
struct in_link *add_in_link(struct graph_pools *pools, struct node *node) {
	struct in_link_chunk *chunk = &node->in;
	u32 index = node->in_count++;
	while (index >= array_count(chunk->links)) {
		index -= array_count(chunk->links);
		if (chunk->next == NULL) {
			chunk->next = alloc(&pools->in_link_pool, sizeof(struct in_link));
		}
		chunk = chunk->next;
	}
	return &chunk->links[index];
}

internal
struct out_link *add_out_link(struct graph_pools *pools, struct node *node) {
	struct out_link_chunk *chunk = &node->out;
	u32 index = node->out_count++;
	while (index >= array_count(chunk->links)) {
		index -= array_count(chunk->links);
		if (chunk->next == NULL) {
			chunk->next = alloc(&pools->out_link_pool, sizeof(struct in_link));
		}
		chunk = chunk->next;
	}
	return &chunk->links[index];
}

struct link {
	struct node *node;
	u32 slot;
	union type *type;
};

// Can't return arrays in C.
struct link2 {
	struct link l[2];
};

struct link3 {
	struct link l[3];
};

struct link5 {
	struct link l[5];
};

internal inline
struct link append_node01(struct graph_pools *pools, u32 *link_id, u8 uop, struct graph *graph, union type *type) {
	struct node *result = alloc_node(pools, uop);
	result->next_constant = graph->constants;
	graph->constants = result;
	OUT0(result).type = type;
	OUT0(result).link_id = (*link_id)++;
	result->out_count = 1;
	return (struct link){result, 0, type};
}

internal inline
struct node *append_node10(struct graph_pools *pools, u8 uop, struct link link) {
	struct node *result = alloc_node(pools, uop);
	IN0(result) = (struct in_link){link.node, link.slot};
	struct out_link *out = out_link(link.node, link.slot);
	out->node = result;
	out->slot = 0;
	result->in_count = 1;
	return result;
}

internal inline
b32 is_constant(u8 uop) {
	return (uop == UOP_UNIT_CONSTANT   ||
	        uop == UOP_VOID_CONSTANT   ||
		uop == UOP_BLOCK_CONSTANT  ||
		uop == UOP_NUMBER_CONSTANT ||
		uop == UOP_TEXT_CONSTANT);
}

internal inline
struct link append_node11(struct graph_pools *pools, u32 *link_id,
                          u8 uop, struct link link, union type *type, b32 optimise) {
	if (optimise) {
		// We ignore assertion errors at this stage. It'll be more
		// convenient to catch them in future passes
		u8 prevuop = link.node->uop;
		if ((uop == UOP_ASSERT_COPYABLE || uop == UOP_ASSERT_DROPPABLE) && is_constant(prevuop)) {
			return link;
		}
		if (uop == UOP_ASSERT_NONZERO && prevuop == UOP_NUMBER_CONSTANT) {
			if (link.node->number != 0) {
				return link;
			}
		}
	}
	struct node *result = append_node10(pools, uop, link);
	OUT0(result).type = type;
	OUT0(result).link_id = (*link_id)++;
	result->out_count = 1;
	return (struct link){result, 0, type};
}

internal inline
struct link2 append_node12(struct graph_pools *pools, u32 *link_id,
                           u8 uop,
                           struct link link,
                           union type *type1, union type *type2,
                           b32 optimise) {
	if (optimise) {
		if (uop == UOP_UNPAIR && link.node->uop == UOP_PAIR) {
			out_link(IN0(link.node).node, IN0(link.node).slot)->node = NULL;
			out_link(IN1(link.node).node, IN1(link.node).slot)->node = NULL;
			return (struct link2){{
				{IN0(link.node).node, IN0(link.node).slot, type1},
				{IN1(link.node).node, IN1(link.node).slot, type2},
			}};
		}
	}
	struct node *result = append_node10(pools, uop, link);
	OUT0(result).type = type1;
	OUT1(result).type = type2;
	OUT0(result).link_id = (*link_id)++;
	OUT1(result).link_id = (*link_id)++;
	result->out_count = 2;
	return (struct link2){{{result, 0, type1}, {result, 1, type2}}};
}

internal inline
struct link3 append_node13(struct graph_pools *pools, u32 *link_id,
                           u8 uop,
                           struct link link,
                           union type *type1, union type *type2, union type *type3,
                           b32 optimise) {
	if (optimise) {
		if (uop == UOP_UNSUM  && link.node->uop == UOP_SUM) {
			out_link(IN0(link.node).node, IN0(link.node).slot)->node = NULL;
			out_link(IN1(link.node).node, IN1(link.node).slot)->node = NULL;
			out_link(IN2(link.node).node, IN2(link.node).slot)->node = NULL;
			return (struct link3){{
				{IN0(link.node).node, IN0(link.node).slot, type1},
				{IN1(link.node).node, IN1(link.node).slot, type2},
				{IN2(link.node).node, IN2(link.node).slot, type2},
			}};
		}
	}
	struct node *result = append_node10(pools, uop, link);
	OUT0(result).type = type1;
	OUT1(result).type = type2;
	OUT2(result).type = type3;
	OUT0(result).link_id = (*link_id)++;
	OUT1(result).link_id = (*link_id)++;
	OUT2(result).link_id = (*link_id)++;
	result->out_count = 3;
	return (struct link3){{{result, 0, type1}, {result, 1, type2}, {result, 2, type3}}};
}

internal inline
struct node *append_node20(struct graph_pools *pools, u8 uop, struct link link1, struct link link2) {
	struct node *result = alloc_node(pools, uop);
	IN0(result).node = link1.node;
	IN1(result).node = link2.node;
	IN0(result).slot = link1.slot;
	IN1(result).slot = link2.slot;
	struct out_link *out1 = out_link(link1.node, link1.slot);
	struct out_link *out2 = out_link(link2.node, link2.slot);
	out1->node = result;
	out2->node = result;
	out1->slot = 0;
	out2->slot = 1;
	result->in_count = 2;
	return result;
}

internal inline
struct link append_node21(struct graph_pools *pools, u32 *link_id,
                          u8 uop,
                          struct link link1, struct link link2,
                          union type *type) {
	struct node *result = append_node20(pools, uop, link1, link2);
	OUT0(result).type = type;
	OUT0(result).link_id = (*link_id)++;
	result->out_count = 1;
	return (struct link){result, 0, type};
}

internal inline
struct link2 append_node22(struct graph_pools *pools, u32 *link_id,
                           u8 uop,
			   struct link link1, struct link link2,
                           union type *type1, union type *type2) {
	struct node *result = append_node20(pools, uop, link1, link2);
	OUT0(result).type = type1;
	OUT1(result).type = type2;
	OUT0(result).link_id = (*link_id)++;
	OUT1(result).link_id = (*link_id)++;
	result->out_count = 2;
	return (struct link2){{{result, 0, type1}, {result, 1, type2}}};
}

internal inline
struct node *append_node30(struct graph_pools *pools, u8 uop, struct link link1, struct link link2, struct link link3) {
	struct node *result = alloc_node(pools, uop);
	IN0(result).node = link1.node;
	IN1(result).node = link2.node;
	IN2(result).node = link3.node;
	IN0(result).slot = link1.slot;
	IN1(result).slot = link2.slot;
	IN2(result).slot = link3.slot;
	struct out_link *out1 = out_link(link1.node, link1.slot);
	struct out_link *out2 = out_link(link2.node, link2.slot);
	struct out_link *out3 = out_link(link3.node, link3.slot);
	out1->node = result;
	out2->node = result;
	out3->node = result;
	out1->slot = 0;
	out2->slot = 1;
	out3->slot = 2;
	result->in_count = 3;
	return result;
}

internal inline
struct link append_node31(struct graph_pools *pools, u32 *link_id,
                          u8 uop,
                          struct link link1, struct link link2, struct link link3,
                          union type *type) {
	struct node *result = append_node30(pools, uop, link1, link2, link3);
	OUT0(result).type = type;
	OUT0(result).link_id = (*link_id)++;
	result->out_count = 1;
	return (struct link){result, 0, type};
}

#define assert_product(type) assert(!IS_VAR(type) && (type)->symbol == SYMBOL_PRODUCT)
#define assert_sum(type)     assert(!IS_VAR(type) && (type)->symbol == SYMBOL_SUM)
#define child1(type) deref((type)->child1)
#define child2(type) deref((type)->child2)

internal inline
struct link2 unpair_(struct graph_pools *pools, u32 *link_id, struct link link, b32 optimise) {
	assert_product(link.type);
	return append_node12(pools, link_id, UOP_UNPAIR, link, child1(link.type), child2(link.type), optimise);
}

internal inline
struct link3 unpair2_(struct graph_pools *pools, u32 *link_id, struct link link, b32 optimise) {
	struct link2 outer = unpair_(pools, link_id, link, optimise);
	struct link2 inner = unpair_(pools, link_id, outer.l[1], optimise);
	return (struct link3){{outer.l[0], inner.l[0], inner.l[1]}};
}

internal inline
struct link pair2_(struct graph_pools *pools, u32 *link_id,
                   struct link link1, struct link link2, struct link link3,
                   union type *type) {
	assert_product(type);
	struct link right = append_node21(pools, link_id, UOP_PAIR, link2, link3, child2(type));
	return append_node21(pools, link_id, UOP_PAIR, link1, right, type);
}

internal inline
struct link3 unsum_(struct graph_pools *pools, u32 *link_id, struct link link, union type *bool_type, b32 optimise) {
	assert_sum(link.type);
	return append_node13(pools, link_id, UOP_UNSUM, link,
	                     child1(link.type), child2(link.type), bool_type,
	                     optimise);
}

internal inline
struct link5 unsum2_(struct graph_pools *pools, u32 *link_id, struct link link, union type *bool_type, b32 optimise) {
	struct link3 outer = unsum_(pools, link_id, link, bool_type, optimise);
	struct link3 inner = unsum_(pools, link_id, outer.l[1], bool_type, optimise);
	return (struct link5){{outer.l[0], inner.l[0], inner.l[1], outer.l[2], inner.l[2]}};
}

internal inline
struct link sum2_(struct graph_pools *pools, u32 *link_id,
                   struct link link1, struct link link2, struct link link3, struct link link4, struct link link5,
                   union type *type) {
	assert_sum(type);
	struct link right = append_node31(pools, link_id, UOP_SUM, link2, link3, link5, child2(type));
	return append_node31(pools, link_id, UOP_SUM, link1, right, link4, type);
}

internal
void build_graph(struct block *block, u32 graph_id, u32 *link_id, union type *bool_type, b32 optimise) {
	if (block->size == 0) {
		return;
	}
	struct graph_pools pools = {};
#define append01(uop, type) append_node01(&pools, link_id, (uop), &block->graph, (type))
#define append10(uop, link) append_node10(&pools, (uop), (link))
#define append11(uop, link, type) append_node11(&pools, link_id, (uop), (link), (type), optimise)
#define append12(uop, link, type1, type2) append_node12(&pools, link_id, (uop), (link), (type1), (type2), optimise)
#define append20(uop, link1, link2) append_node20(&pools, (uop), (link1), (link2))
#define append21(uop, link1, link2, type) \
	append_node21(&pools, link_id, (uop), (link1), (link2), (type))
#define append22(uop, link1, link2, type1, type2) \
	append_node22(&pools, link_id, (uop), (link1), (link2), (type1), (type2))
#define append31(uop, link1, link2, link3, type) \
	append_node31(&pools, link_id, (uop), (link1), (link2), (link3), (type))
#define unpair(link) unpair_(&pools, link_id, (link), optimise)
#define unpair2(link) unpair2_(&pools, link_id, (link), optimise)
#define pair(link1, link2, type) append21(UOP_PAIR, (link1), (link2), (type))
#define pair2(link1, link2, link3, type) \
	pair2_(&pools, link_id, (link1), (link2), (link3), (type))
#define unsum(link) unsum_(&pools, link_id, (link), bool_type, optimise)
#define unsum2(link) unsum2_(&pools, link_id, (link), bool_type, optimise)
#define sum(link1, link2, link3, type) append31(UOP_SUM, (link1), (link2), (link3), (type))
#define sum2(link1, link2, link3, link4, link5, type) \
	sum2_(&pools, link_id, (link1), (link2), (link3), (link4), (link5), (type))
#define and(link1, link2) append21(UOP_AND, (link1), (link2), bool_type)
#define or(link1, link2) append21(UOP_OR, (link1), (link2), bool_type)
#define not(link) append11(UOP_NOT, (link), bool_type)
	block->graph.id = graph_id;
	OUT0(&block->graph.input).type = deref(block->types[0]);
	OUT0(&block->graph.input).link_id = (*link_id)++;
	block->graph.input.out_count = 1;
	struct link last = {&block->graph.input, 0, OUT0(&block->graph.input).type};
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
					// Only set the seal if the node wasn't optimised away
					if (last.node->uop == UOP_SEAL && last.node->seal == NULL) {
						last.node->seal = block->sealers[sealer_index++];
					}
					break;
				}
			case OP_UNSEAL:
				{
					last = append11(UOP_UNSEAL, last, output_type);
					// Only set the seal if the node wasn't optimised away
					if (last.node->uop == UOP_UNSEAL && last.node->seal == NULL) {
						last.node->seal = block->sealers[sealer_index++];
					}
					break;
				}
			case OP_ASSERT_EQUAL:
				{
					struct link3 input = unpair2(last);
					struct link2 assert_equal = append22(UOP_ASSERT_EQUAL,
					                                     input.l[0], input.l[1],
					                                     input.l[0].type, input.l[1].type);
					last = pair2(assert_equal.l[0], assert_equal.l[1], input.l[2], output_type);
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
					struct link3 input = unpair2(last);
					assert_product(output_type);
					struct link left = pair(input.l[0], input.l[1], child1(output_type));
					last = pair(left, input.l[2], output_type);
					break;
				}
			case 'r':
				{
					struct link2 outer = unpair(last);
					struct link2 inner = unpair(outer.l[0]);
					last = pair2(inner.l[0], inner.l[1], outer.l[1], output_type);
					break;
				}
			case 'w':
				{
					struct link3 input = unpair2(last);
					last = pair2(input.l[1], input.l[0], input.l[2], output_type);
					break;
				}
			case 'z':
				{
					struct link3 input = unpair2(last);
					struct link2 inner = unpair(input.l[2]);
					assert_product(output_type);
					union type *type = child2(output_type);
					assert_product(type);
					struct link new_inner = pair(input.l[1], inner.l[1], child2(type));
					last = pair2(input.l[0], inner.l[0], new_inner, output_type);
					break;
				}
			case 'v':
				{
					assert_product(output_type);
					struct link unit_constant = append01(UOP_UNIT_CONSTANT,
					                                     child2(output_type));
					last = pair(last, unit_constant, output_type);
					break;
				}
			case 'c':
				{
					struct link2 input = unpair(last);
					append10(UOP_DROP, input.l[1]);
					last = input.l[0];
					break;
				}
			case '%':
				{
					struct link2 input = unpair(last);
					struct link to_drop = append11(UOP_ASSERT_DROPPABLE,
					                               input.l[0], input.l[0].type);
					append10(UOP_DROP, to_drop);
					last = input.l[1];
					break;
				}
			case '^':
				{
					struct link2 input = unpair(last);
					struct link to_copy = append11(UOP_ASSERT_COPYABLE,
					                               input.l[0], input.l[0].type);
					struct link2 copy = append12(UOP_COPY, to_copy,
					                             to_copy.type, to_copy.type);
					last = pair2(copy.l[0], copy.l[1], input.l[1], output_type);
					break;
				}

			case '$':
				{
					struct link3 input = unpair2(last);
					assert_product(output_type);
					struct link apply = append21(UOP_APPLY,
					                             input.l[0], input.l[1],
					                             child1(output_type));
					last = pair(apply, input.l[2], output_type);
					break;
				}
			case 'o':
				{
					struct link3 input = unpair2(last);
					assert_product(output_type);
					struct link compose = append21(UOP_COMPOSE,
					                               input.l[0], input.l[1],
					                               child1(output_type));
					last = pair(compose, input.l[2], output_type);
					break;
				}
			case '\'':
				{
					struct link2 input = unpair(last);
					assert_product(output_type);
					struct link quote = append11(UOP_QUOTE, input.l[0], child1(output_type));
					last = pair(quote, input.l[1], output_type);
					break;
				}
			case 'k':
				{
					struct link2 input = unpair(last);
					assert_product(output_type);
					struct link relevant = append11(UOP_MARK_RELEVANT,
					                                input.l[0], child1(output_type));
					last = pair(relevant, input.l[1], output_type);
					break;
				}
			case 'f':
				{
					struct link2 input = unpair(last);
					assert_product(output_type);
					struct link affine = append11(UOP_MARK_AFFINE,
					                              input.l[0], child1(output_type));
					last = pair(affine, input.l[1], output_type);
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
					struct link2 input = unpair(last);
					if (input.l[0].node->uop == UOP_NUMBER_CONSTANT) {
						input.l[0].node->number = input.l[0].node->number * 10 + digit;
						break;
					}
					assert_product(output_type);
					union type *number = child1(output_type);
					struct link ten_constant = append01(UOP_NUMBER_CONSTANT, number);
					ten_constant.node->number = 10;
					struct link digit_constant = append01(UOP_NUMBER_CONSTANT, number);
					digit_constant.node->number = digit;
					struct link mul_by_10 = append21(UOP_MULTIPLY,
					                                 input.l[0], ten_constant, number);
					struct link add_digit = append21(UOP_ADD,
					                                 mul_by_10, digit_constant, number);
					last = pair(add_digit, input.l[1], output_type);
					break;
				}

			case '+':
				{
					struct link3 input = unpair2(last);
					assert_product(output_type);
					struct link add = append21(UOP_ADD, input.l[0], input.l[1], child1(output_type));
					last = pair(add, input.l[2], output_type);
					break;
				}
			case '*':
				{
					struct link3 input = unpair2(last);
					assert_product(output_type);
					struct link multiply = append21(UOP_MULTIPLY,
					                                input.l[0], input.l[1], child1(output_type));
					last = pair(multiply, input.l[2], output_type);
					break;
				}
			case '/':
				{
					struct link2 input = unpair(last);
					assert_product(output_type);
					union type *number = child1(output_type);
					struct link checked_number = append11(UOP_ASSERT_NONZERO, input.l[0], number);
					struct link inverse = append11(UOP_INVERSE, checked_number, number);
					last = pair(inverse, input.l[1], output_type);
					break;
				}
			case '-':
				{
					struct link2 input = unpair(last);
					assert_product(output_type);
					struct link negate = append11(UOP_NEGATE, input.l[0], child1(output_type));
					last = pair(negate, input.l[1], output_type);
					break;
				}
			case 'Q':
				{
					struct link3 input = unpair2(last);
					assert_product(output_type);
					union type *number = child1(output_type);
					struct link checked_number = append11(UOP_ASSERT_NONZERO, input.l[0], number);
					struct link2 divmod = append22(UOP_DIVMOD,
					                               checked_number, input.l[1],
					                               number, number);
					last = pair2(divmod.l[0], divmod.l[1], input.l[2], output_type);
					break;
				}

			// The condition equations are listed for the output
			// nodes, in outside-in order. The variables a, b, c
			// represent the conditions of the input nodes, in
			// outside-in order (e.g. a is the condition of the
			// outermost node in the input).
			case 'L':
				{
					// Conditions:
					// a || b
					// a
					struct link2 input = unpair(last);
					struct link5 branches = unsum2(input.l[0]);
					assert_product(output_type);
					union type *type = child1(output_type);
					struct link2 cond_inner = append12(UOP_COPY, branches.l[3],
					                               bool_type, bool_type);
					assert_sum(type);
					struct link inner = sum(branches.l[0], branches.l[1], cond_inner.l[0],
					                        child1(type));
					struct link cond_outer = or(cond_inner.l[1], branches.l[4]);
					struct link outer = sum(inner, branches.l[2], cond_outer, type);
					last = pair(outer, input.l[1], output_type);
					break;
				}
			case 'R':
				{
					// Conditions:
					// a && b
					// a && !b
					struct link2 input = unpair(last);
					struct link3 outer = unsum(input.l[0]);
					struct link3 inner = unsum(outer.l[0]);
					struct link2 cond_outer = append12(UOP_COPY, outer.l[2],
					                                   bool_type, bool_type);
					struct link2 cond_inner = append12(UOP_COPY, inner.l[2],
					                                   bool_type, bool_type);
					assert_product(output_type);
					struct link new_outer = sum2(inner.l[0], inner.l[1], outer.l[1],
					                             and(cond_outer.l[0], cond_inner.l[0]),
					                             and(cond_outer.l[1], not(cond_inner.l[1])),
					                             child1(output_type));
					last = pair(new_outer, input.l[1], output_type);
					break;
				}
			case 'W':
				{
					// Conditions:
					// !a && b
					// a
					struct link2 input = unpair(last);
					struct link5 branches = unsum2(input.l[0]);
					struct link2 cond_outer = append12(UOP_COPY, branches.l[3],
					                                   bool_type, bool_type);
					assert_product(output_type);
					struct link new_branches = sum2(branches.l[1], branches.l[0], branches.l[2],
					                                and(not(cond_outer.l[0]), branches.l[4]),
					                                cond_outer.l[1],
					                                child1(output_type));
					last = pair(new_branches, input.l[1], output_type);
					break;
				}
			case 'Z':
				{
					// Conditions:
					// a
					// !a && !b && c
					// !a && b
					struct link2 input = unpair(last);
					struct link5 branches = unsum2(input.l[0]);
					struct link3 inner = unsum(branches.l[2]);
					struct link2 cond_outer = append12(UOP_COPY, branches.l[3],
					                                   bool_type, bool_type);
					struct link2 not_cond_outer = append12(UOP_COPY, not(cond_outer.l[0]),
					                                       bool_type, bool_type);
					struct link2 cond_middle = append12(UOP_COPY, branches.l[4],
					                                    bool_type, bool_type);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					union type *type2 = child2(type);
					assert_sum(type2);
					struct link new_inner = sum(branches.l[1], inner.l[1],
					                            and(not_cond_outer.l[0], cond_middle.l[0]),
					                            child2(type2));
					struct link new_branches = sum2(branches.l[0], branches.l[2], new_inner,
					                                cond_outer.l[1],
					                                and(not_cond_outer.l[1],
					                                    and(not(cond_middle.l[1]), inner.l[2])),
					                                type);
					last = pair(new_branches, input.l[1], output_type);
					break;
				}
			case 'V':
				{
					struct link2 input = unpair(last);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					struct link void_constant = append01(UOP_VOID_CONSTANT, child2(type));
					struct link bool_constant = append01(UOP_BOOL_CONSTANT, bool_type);
					bool_constant.node->boolean = true;
					struct link branches = sum(input.l[0], void_constant, bool_constant, type);
					last = pair(branches, input.l[1], output_type);
					break;
				}
			case 'C':
				{
					struct link2 input = unpair(last);
					struct link3 branches = unsum(input.l[0]);
					append10(UOP_DROP, branches.l[1]);
					append10(UOP_DROP, branches.l[2]);
					last = pair(branches.l[0], input.l[1], output_type);
					break;
				}

			case '?':
				{
					struct link3 input = unpair2(last);
					struct link checked_block = append11(UOP_ASSERT_DROPPABLE,
					                                     input.l[0], input.l[0].type);
					struct link3 branches = unsum(input.l[1]);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					struct link applied_branch = append21(UOP_APPLY,
					                                      checked_block, branches.l[0],
					                                      child1(type));
					struct link new_branches = sum(applied_branch, branches.l[1], branches.l[2],
					                               type);
					last = pair(new_branches, input.l[2], output_type);
					break;
				}
			case 'D':
				{
					struct link3 input = unpair2(last);
					struct link3 branches = unsum(input.l[1]);
					struct link2 cond = append12(UOP_COPY, branches.l[2], bool_type, bool_type);
					struct link2 distrib = append22(UOP_DISTRIB, input.l[0], cond.l[0],
					                                input.l[0].type, input.l[0].type);
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					struct link left = pair(distrib.l[0], branches.l[0],
					                        child1(type));
					struct link right = pair(distrib.l[1], branches.l[1],
								 child2(type));
					struct link new_branches = sum(left, right, cond.l[1], type);
					last = pair(new_branches, input.l[2], output_type);
					break;
				}
			case 'F':
				{
					struct link2 input = unpair(last);
					struct link3 branches = unsum(input.l[0]);
					struct link2 cond = append12(UOP_COPY, branches.l[2], bool_type, bool_type);
					struct link2 left = unpair(branches.l[0]);
					struct link2 right = unpair(branches.l[1]);
					assert_product(output_type);
					union type *type = child2(output_type);
					assert_product(type);
					struct link new_left = sum(left.l[0], right.l[0], cond.l[0],
					                           child1(output_type));
					struct link new_right = sum(left.l[1], right.l[1], cond.l[1],
					                           child1(type));
					last = pair2(new_left, new_right, input.l[1], output_type);
					break;
				}
			case 'M':
				{
					struct link2 input = unpair(last);
					struct link3 branches = unsum(input.l[0]);
					assert_product(output_type);
					struct link merge = append31(UOP_MERGE, branches.l[0], branches.l[1],
					                             branches.l[2],
					                             child1(output_type));
					last = pair(merge, input.l[1], output_type);
					break;
				}
			case 'K':
				{
					struct link2 input = unpair(last);
					struct link3 branches = unsum(input.l[0]);
					append20(UOP_ASSERT_VOID, branches.l[0], branches.l[2]);
					last = pair(branches.l[1], input.l[1], output_type);
					break;
				}

			case '>':
				{
					struct link3 input = unpair2(last);
					struct node *greater = append_node20(&pools,
					                                     UOP_GREATER, input.l[1], input.l[0]);
					struct link g0 = {greater, 0, input.l[0].type};
					struct link g1 = {greater, 1, input.l[1].type};
					struct link g2 = {greater, 2, input.l[1].type};
					struct link g3 = {greater, 3, input.l[0].type};
					struct link g4 = {greater, 4, bool_type};
					greater->out.next = alloc(&pools.out_link_pool,
					                          sizeof(struct out_link_chunk));
					OUT0(greater).type = g0.type;
					OUT1(greater).type = g1.type;
					OUT2(greater).type = g2.type;
					OUT3(greater).type = g3.type;
					OUT4(greater).type = g4.type;
					OUT0(greater).link_id = (*link_id)++;
					OUT1(greater).link_id = (*link_id)++;
					OUT2(greater).link_id = (*link_id)++;
					OUT3(greater).link_id = (*link_id)++;
					OUT4(greater).link_id = (*link_id)++;
					greater->out_count = 5;
					assert_product(output_type);
					union type *type = child1(output_type);
					assert_sum(type);
					// We have truth on left in the graph IR
					struct link right = pair(g0, g1, child1(type));
					struct link left  = pair(g2, g3, child2(type));
					struct link branches = sum(left, right, not(g4), type);
					last = pair(branches, input.l[2], output_type);
					break;
				}

			default:
				{
					assert(!"Received invalid opcode");
				}
		}
	}
	last.node->out.links[last.slot].node = &block->graph.output;
	last.node->out.links[last.slot].slot = 0;
	block->graph.output.uop = UOP_END;
	block->graph.output.in_count = 1;
	IN0(&block->graph.output).node = last.node;
	IN0(&block->graph.output).slot = last.slot;
}

// TODO move the link_id stuff into generate_c
internal
void build_graphs(struct block_ptr_array blocks, union type *bool_type, b32 optimise) {
	u32 link_id = 0;
	foreach (block, blocks) {
		build_graph(*block, (u32)(blocks.size - block_index - 1), &link_id, bool_type, optimise);
	}
}
