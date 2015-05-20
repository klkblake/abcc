#include "infer_types.gen.c"

internal
Node *alloc_node(Graph *graph, enum uop uop) {
	Node *result = alloc(&graph->node_pool, sizeof(Node));
	result->uop = uop;
	return result;
}

internal
void delete_constant(Graph *graph, Node *node) {
	if (graph->constants == node) {
		graph->constants = node->next_constant;
	} else {
		Node *constant = graph->constants;
		while (constant) {
			if (constant->next_constant == node) {
				constant->next_constant = node->next_constant;
				break;
			}
			constant = constant->next_constant;
		}
	}
	// TODO Add to free list
}

internal
InLink *in_link(Node *node, u32 index) {
	InLinkChunk *chunk = &node->in;
	while (index >= array_count(chunk->links)) {
		index -= array_count(chunk->links);
		chunk = chunk->next;
	}
	return &chunk->links[index];
}

internal
OutLink *out_link(Node *node, u32 index) {
	OutLinkChunk *chunk = &node->out;
	while (index >= array_count(chunk->links)) {
		index -= array_count(chunk->links);
		chunk = chunk->next;
	}
	return &chunk->links[index];
}

internal
InLink *add_in_link(Graph *graph, Node *node) {
	InLinkChunk *chunk = &node->in;
	u32 index = node->in_count++;
	while (index >= array_count(chunk->links)) {
		index -= array_count(chunk->links);
		if (chunk->next == NULL) {
			chunk->next = alloc(&graph->in_link_pool, sizeof(InLink));
		}
		chunk = chunk->next;
	}
	return &chunk->links[index];
}

internal
OutLink *add_out_link(Graph *graph, Node *node) {
	OutLinkChunk *chunk = &node->out;
	u32 index = node->out_count++;
	while (index >= array_count(chunk->links)) {
		index -= array_count(chunk->links);
		if (chunk->next == NULL) {
			chunk->next = alloc(&graph->out_link_pool, sizeof(InLink));
		}
		chunk = chunk->next;
	}
	return &chunk->links[index];
}

// TODO rename this to Graph, rename Graph to Subgraph(?)
typedef struct {
	GraphPtrArray graphs;
	Graph *first;
	Graph *last;
	b32 optimise;
	Pool graph_pool;
} GraphState;

typedef struct {
	Node *node;
	u32 slot;
	Type *type;
} Link;

// Can't return arrays in C.
typedef struct {
	Link l[2];
} Link2;

typedef struct {
	Link l[3];
} Link3;

typedef struct {
	Link l[5];
} Link5;

#define assert_product(type) assert(!IS_VAR(type) && (type)->symbol == SYMBOL_PRODUCT)
#define assert_sum(type)     assert(!IS_VAR(type) && (type)->symbol == SYMBOL_SUM)
#define assert_block(type)   assert(!IS_VAR(type) && ((type)->symbol & POLYMORPHIC_MASK) == SYMBOL_BLOCK)
#define child1(type) deref((type)->child1)
#define child2(type) deref((type)->child2)

// We need this to construct pairs for some of the optimisations
internal Link append_node21(GraphState *state, Graph *graph, u8 uop, Link link1, Link link2, Type *type);

internal inline
Link append_node01(GraphState *state, Graph *graph, u8 uop, Type *type) {
	Node *result = alloc_node(graph, uop);
	result->next_constant = graph->constants;
	graph->constants = result;
	OUT0(result).type = type;
	result->out_count = 1;
	return (Link){result, 0, type};
}

internal inline
Node *append_node10(Graph *graph, u8 uop, Link link) {
	Node *result = alloc_node(graph, uop);
	IN0(result) = (InLink){link.node, link.slot};
	OutLink *out = out_link(link.node, link.slot);
	out->node = result;
	out->slot = 0;
	result->in_count = 1;
	return result;
}

internal inline
Link append_node11(GraphState *state, Graph *graph, u8 uop, Link link, Type *type) {
	if (state->optimise) {
		// We ignore assertion errors at this stage. It'll be more
		// convenient to catch them in future passes
		u8 prevuop = link.node->uop;
		if ((uop == UOP_ASSERT_COPYABLE || uop == UOP_ASSERT_DROPPABLE) && is_constant(prevuop)) {
			return link;
		}
		if (uop == UOP_ASSERT_NONZERO && prevuop == UOP_NUMBER_CONSTANT) {
			// XXX This is bogus, but it doesn't matter because it
			// will be replaced by exact rationals soon anyway.
			if (link.node->number > 0.00000000001f || link.node->number < -0.00000000001f) {
				return link;
			}
		}
		if (uop == UOP_QUOTE && prevuop == UOP_BLOCK_CONSTANT) {
			Graph *quoted = link.node->block->quoted;
			if (quoted) {
				link.node->block = quoted;
				return link;
			}
			assert_block(type);
			Type *input_type = child1(type);
			Type *output_type = child2(type);
			assert_product(output_type);
			quoted = alloc(&state->graph_pool, sizeof(Graph));
			if (graph->prev) {
				quoted->prev = graph->prev;
				graph->prev->next = quoted;
			} else {
				state->first = quoted;
			}
			quoted->next = graph;
			graph->prev = quoted;
			quoted->id = (u32)state->graphs.size;
			array_push(&state->graphs, quoted);
			quoted->input.out_count = 1;
			OUT0(&quoted->input).type = input_type;
			Link input = {&quoted->input, 0, input_type};
			Link block = append_node01(state, quoted, UOP_BLOCK_CONSTANT, child1(output_type));
			block.node->block = link.node->block;
			Link output = append_node21(state, quoted, UOP_PAIR, block, input, output_type);
			OUT0(output.node).node = &quoted->output;
			OUT0(output.node).slot = 0;
			IN0(&quoted->output).node = output.node;
			IN0(&quoted->output).slot = output.slot;
			quoted->output.uop = UOP_END;
			quoted->output.in_count = 1;
			link.node->block->quoted = quoted;
			link.node->block = quoted;
			link.type = type;
			OUT0(link.node).type = type;
			return link;
		}
	}
	Node *result = append_node10(graph, uop, link);
	OUT0(result).type = type;
	result->out_count = 1;
	return (Link){result, 0, type};
}

internal inline
Link2 append_node12(GraphState *state, Graph *graph,
                    u8 uop,
                    Link link,
                    Type *type1, Type *type2) {
	if (state->optimise) {
		if (uop == UOP_UNPAIR && link.node->uop == UOP_PAIR) {
			out_link(IN0(link.node).node, IN0(link.node).slot)->node = NULL;
			out_link(IN1(link.node).node, IN1(link.node).slot)->node = NULL;
			return (Link2){{
				{IN0(link.node).node, IN0(link.node).slot, type1},
				{IN1(link.node).node, IN1(link.node).slot, type2},
			}};
		}
		if (uop == UOP_COPY) {
			if (is_constant(link.node->uop)) {
				Link copy = append_node01(state, graph, link.node->uop, link.type);
				copy.node->text = link.node->text;
				return (Link2){{link, copy}};
			}
			if (does_implicit_copies(link.node->uop)) {
				OutLink *copy = add_out_link(graph, link.node);
				copy->type = type2;
				return (Link2){{link, {link.node, link.node->out_count - 1, type2}}};
			}
		}
	}
	Node *result = append_node10(graph, uop, link);
	OUT0(result).type = type1;
	OUT1(result).type = type2;
	result->out_count = 2;
	return (Link2){{{result, 0, type1}, {result, 1, type2}}};
}

internal inline
Link3 append_node13(GraphState *state, Graph *graph,
                    u8 uop,
                    Link link,
                    Type *type1, Type *type2, Type *type3) {
	if (state->optimise) {
		if (uop == UOP_UNSUM  && link.node->uop == UOP_SUM) {
			out_link(IN0(link.node).node, IN0(link.node).slot)->node = NULL;
			out_link(IN1(link.node).node, IN1(link.node).slot)->node = NULL;
			out_link(IN2(link.node).node, IN2(link.node).slot)->node = NULL;
			return (Link3){{
				{IN0(link.node).node, IN0(link.node).slot, type1},
				{IN1(link.node).node, IN1(link.node).slot, type2},
				{IN2(link.node).node, IN2(link.node).slot, type2},
			}};
		}
	}
	Node *result = append_node10(graph, uop, link);
	OUT0(result).type = type1;
	OUT1(result).type = type2;
	OUT2(result).type = type3;
	result->out_count = 3;
	return (Link3){{{result, 0, type1}, {result, 1, type2}, {result, 2, type3}}};
}

internal inline
Node *append_node20(Graph *graph, u8 uop, Link link1, Link link2) {
	Node *result = alloc_node(graph, uop);
	IN0(result).node = link1.node;
	IN1(result).node = link2.node;
	IN0(result).slot = link1.slot;
	IN1(result).slot = link2.slot;
	OutLink *out1 = out_link(link1.node, link1.slot);
	OutLink *out2 = out_link(link2.node, link2.slot);
	out1->node = result;
	out2->node = result;
	out1->slot = 0;
	out2->slot = 1;
	result->in_count = 2;
	return result;
}

internal inline
Link append_node21(GraphState *state, Graph *graph,
                   u8 uop,
                   Link link1, Link link2,
                   Type *type) {
	if (state->optimise) {
		if (uop == UOP_AND) {
			if (link1.node->uop == UOP_BOOL_CONSTANT && link1.node->boolean) {
				delete_constant(graph, link1.node);
				return link2;
			}
			if (link2.node->uop == UOP_BOOL_CONSTANT && link2.node->boolean) {
				delete_constant(graph, link2.node);
				return link1;
			}
			if (link1.node->uop == UOP_BOOL_CONSTANT && !link1.node->boolean) {
				append_node10(graph, UOP_DROP, link2);
				return link1;
			}
			if (link2.node->uop == UOP_BOOL_CONSTANT && !link2.node->boolean) {
				append_node10(graph, UOP_DROP, link1);
				return link2;
			}
		}
	}
	Node *result = append_node20(graph, uop, link1, link2);
	OUT0(result).type = type;
	result->out_count = 1;
	return (Link){result, 0, type};
}

internal inline
Link2 append_node22(GraphState *state, Graph *graph,
                    u8 uop,
                    Link link1, Link link2,
                    Type *type1, Type *type2) {
	Node *result = append_node20(graph, uop, link1, link2);
	OUT0(result).type = type1;
	OUT1(result).type = type2;
	result->out_count = 2;
	return (Link2){{{result, 0, type1}, {result, 1, type2}}};
}

internal inline
Node *append_node30(Graph *graph, u8 uop, Link link1, Link link2, Link link3) {
	Node *result = alloc_node(graph, uop);
	IN0(result).node = link1.node;
	IN1(result).node = link2.node;
	IN2(result).node = link3.node;
	IN0(result).slot = link1.slot;
	IN1(result).slot = link2.slot;
	IN2(result).slot = link3.slot;
	OutLink *out1 = out_link(link1.node, link1.slot);
	OutLink *out2 = out_link(link2.node, link2.slot);
	OutLink *out3 = out_link(link3.node, link3.slot);
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
Link append_node31(GraphState *state, Graph *graph,
                   u8 uop,
                   Link link1, Link link2, Link link3,
                   Type *type) {
	Node *result = append_node30(graph, uop, link1, link2, link3);
	OUT0(result).type = type;
	result->out_count = 1;
	return (Link){result, 0, type};
}

internal inline
Link2 unpair_(GraphState *state, Graph *graph, Link link) {
	assert_product(link.type);
	return append_node12(state, graph, UOP_UNPAIR, link, child1(link.type), child2(link.type));
}

internal inline
Link3 unpair2_(GraphState *state, Graph *graph, Link link) {
	Link2 outer = unpair_(state, graph, link);
	Link2 inner = unpair_(state, graph, outer.l[1]);
	return (Link3){{outer.l[0], inner.l[0], inner.l[1]}};
}

internal inline
Link pair2_(GraphState *state, Graph *graph,
            Link link1, Link link2, Link link3,
            Type *type) {
	assert_product(type);
	Link right = append_node21(state, graph, UOP_PAIR, link2, link3, child2(type));
	return append_node21(state, graph, UOP_PAIR, link1, right, type);
}

internal inline
Link3 unsum_(GraphState *state, Graph *graph, Link link, Type *bool_type) {
	assert_sum(link.type);
	return append_node13(state, graph, UOP_UNSUM, link,
	                     child1(link.type), child2(link.type), bool_type);
}

internal inline
Link5 unsum2_(GraphState *state, Graph *graph, Link link, Type *bool_type) {
	Link3 outer = unsum_(state, graph, link, bool_type);
	Link3 inner = unsum_(state, graph, outer.l[1], bool_type);
	return (Link5){{outer.l[0], inner.l[0], inner.l[1], outer.l[2], inner.l[2]}};
}

internal inline
Link sum2_(GraphState *state, Graph *graph,
           Link link1, Link link2, Link link3, Link link4, Link link5,
           Type *type) {
	assert_sum(type);
	Link right = append_node31(state, graph, UOP_SUM, link2, link3, link5, child2(type));
	return append_node31(state, graph, UOP_SUM, link1, right, link4, type);
}

internal
void build_graph(GraphState *state, Block *block, Type *bool_type) {
	if (block->size == 0) {
		return;
	}
	Graph *graph = &block->graph;
#define append01(uop, type) append_node01(state, graph, (uop), (type))
#define append10(uop, link) append_node10(graph, (uop), (link))
#define append11(uop, link, type) append_node11(state, graph, (uop), (link), (type))
#define append12(uop, link, type1, type2) append_node12(state, graph, (uop), (link), (type1), (type2))
#define append20(uop, link1, link2) append_node20(graph, (uop), (link1), (link2))
#define append21(uop, link1, link2, type) \
	append_node21(state, graph, (uop), (link1), (link2), (type))
#define append22(uop, link1, link2, type1, type2) \
	append_node22(state, graph, (uop), (link1), (link2), (type1), (type2))
#define append31(uop, link1, link2, link3, type) \
	append_node31(state, graph, (uop), (link1), (link2), (link3), (type))
#define unpair(link) unpair_(state, graph, (link))
#define unpair2(link) unpair2_(state, graph, (link))
#define pair(link1, link2, type) append21(UOP_PAIR, (link1), (link2), (type))
#define pair2(link1, link2, link3, type) \
	pair2_(state, graph, (link1), (link2), (link3), (type))
#define unsum(link) unsum_(state, graph, (link), bool_type)
#define unsum2(link) unsum2_(state, graph, (link), bool_type)
#define sum(link1, link2, link3, type) append31(UOP_SUM, (link1), (link2), (link3), (type))
#define sum2(link1, link2, link3, link4, link5, type) \
	sum2_(state, graph, (link1), (link2), (link3), (link4), (link5), (type))
#define and(link1, link2) append21(UOP_AND, (link1), (link2), bool_type)
#define or(link1, link2) append21(UOP_OR, (link1), (link2), bool_type)
#define not(link) append11(UOP_NOT, (link), bool_type)
	if (state->last) {
		state->last->next = graph;
		graph->prev = state->last;
	} else {
		state->first = graph;
	}
	state->last = graph;
	graph->id = (u32)state->graphs.size;
	array_push(&state->graphs, graph);
	OUT0(&graph->input).type = deref(block->types[0]);
	graph->input.out_count = 1;
	Link last = {&graph->input, 0, OUT0(&graph->input).type};
	AOStackFrame *frame = NULL;
	for (usize i = 0, frame_index = 0, block_index = 0, text_index = 0, sealer_index = 0; i < block->size; i++) {
		u8 op = block->opcodes[i];
		Type *output_type = deref(block->types[i + 1]);
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
					Link3 input = unpair2(last);
					Link2 assert_equal = append22(UOP_ASSERT_EQUAL,
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
					Link block_constant = append01(UOP_BLOCK_CONSTANT,
					                                      child1(output_type));
					block_constant.node->block = &block->blocks[block_index++]->graph;
					last = pair(block_constant, last, output_type);
					break;
				}
			case '"':
				{
					assert_product(output_type);
					Link text_constant = append01(UOP_TEXT_CONSTANT,
					                                     child1(output_type));
					text_constant.node->text = block->texts[text_index++];
					last = pair(text_constant, last, output_type);
					break;
				}

			case 'l':
				{
					Link3 input = unpair2(last);
					assert_product(output_type);
					Link left = pair(input.l[0], input.l[1], child1(output_type));
					last = pair(left, input.l[2], output_type);
					break;
				}
			case 'r':
				{
					Link2 outer = unpair(last);
					Link2 inner = unpair(outer.l[0]);
					last = pair2(inner.l[0], inner.l[1], outer.l[1], output_type);
					break;
				}
			case 'w':
				{
					Link3 input = unpair2(last);
					last = pair2(input.l[1], input.l[0], input.l[2], output_type);
					break;
				}
			case 'z':
				{
					Link3 input = unpair2(last);
					Link2 inner = unpair(input.l[2]);
					assert_product(output_type);
					Type *type = child2(output_type);
					assert_product(type);
					Link new_inner = pair(input.l[1], inner.l[1], child2(type));
					last = pair2(input.l[0], inner.l[0], new_inner, output_type);
					break;
				}
			case 'v':
				{
					assert_product(output_type);
					Link unit_constant = append01(UOP_UNIT_CONSTANT,
					                                     child2(output_type));
					last = pair(last, unit_constant, output_type);
					break;
				}
			case 'c':
				{
					Link2 input = unpair(last);
					append10(UOP_DROP, input.l[1]);
					last = input.l[0];
					break;
				}
			case '%':
				{
					Link2 input = unpair(last);
					Link to_drop = append11(UOP_ASSERT_DROPPABLE,
					                               input.l[0], input.l[0].type);
					append10(UOP_DROP, to_drop);
					last = input.l[1];
					break;
				}
			case '^':
				{
					Link2 input = unpair(last);
					Link to_copy = append11(UOP_ASSERT_COPYABLE,
					                               input.l[0], input.l[0].type);
					Link2 copy = append12(UOP_COPY, to_copy,
					                             to_copy.type, to_copy.type);
					last = pair2(copy.l[0], copy.l[1], input.l[1], output_type);
					break;
				}

			case '$':
				{
					Link3 input = unpair2(last);
					assert_product(output_type);
					Link apply = append21(UOP_APPLY,
					                             input.l[0], input.l[1],
					                             child1(output_type));
					last = pair(apply, input.l[2], output_type);
					break;
				}
			case 'o':
				{
					Link3 input = unpair2(last);
					assert_product(output_type);
					Link compose = append21(UOP_COMPOSE,
					                               input.l[0], input.l[1],
					                               child1(output_type));
					last = pair(compose, input.l[2], output_type);
					break;
				}
			case '\'':
				{
					Link2 input = unpair(last);
					assert_product(output_type);
					Link quote = append11(UOP_QUOTE, input.l[0], child1(output_type));
					last = pair(quote, input.l[1], output_type);
					break;
				}
			case 'k':
				{
					Link2 input = unpair(last);
					assert_product(output_type);
					Link relevant = append11(UOP_MARK_RELEVANT,
					                                input.l[0], child1(output_type));
					last = pair(relevant, input.l[1], output_type);
					break;
				}
			case 'f':
				{
					Link2 input = unpair(last);
					assert_product(output_type);
					Link affine = append11(UOP_MARK_AFFINE,
					                              input.l[0], child1(output_type));
					last = pair(affine, input.l[1], output_type);
					break;
				}

			case '#':
				{
					assert_product(output_type);
					Link zero_constant = append01(UOP_NUMBER_CONSTANT,
					                                     child1(output_type));
					zero_constant.node->number = 0;
					last = pair(zero_constant, last, output_type);
					break;
				}
			case '0'...'9':
				{
					u32 digit = op - '0';
					Link2 input = unpair(last);
					if (input.l[0].node->uop == UOP_NUMBER_CONSTANT) {
						input.l[0].node->number = input.l[0].node->number * 10 + digit;
						break;
					}
					assert_product(output_type);
					Type *number = child1(output_type);
					Link ten_constant = append01(UOP_NUMBER_CONSTANT, number);
					ten_constant.node->number = 10;
					Link digit_constant = append01(UOP_NUMBER_CONSTANT, number);
					digit_constant.node->number = digit;
					Link mul_by_10 = append21(UOP_MULTIPLY,
					                                 input.l[0], ten_constant, number);
					Link add_digit = append21(UOP_ADD,
					                                 mul_by_10, digit_constant, number);
					last = pair(add_digit, input.l[1], output_type);
					break;
				}

			case '+':
				{
					Link3 input = unpair2(last);
					assert_product(output_type);
					Link add = append21(UOP_ADD, input.l[0], input.l[1], child1(output_type));
					last = pair(add, input.l[2], output_type);
					break;
				}
			case '*':
				{
					Link3 input = unpair2(last);
					assert_product(output_type);
					Link multiply = append21(UOP_MULTIPLY,
					                                input.l[0], input.l[1], child1(output_type));
					last = pair(multiply, input.l[2], output_type);
					break;
				}
			case '/':
				{
					Link2 input = unpair(last);
					assert_product(output_type);
					Type *number = child1(output_type);
					Link checked_number = append11(UOP_ASSERT_NONZERO, input.l[0], number);
					Link inverse = append11(UOP_INVERSE, checked_number, number);
					last = pair(inverse, input.l[1], output_type);
					break;
				}
			case '-':
				{
					Link2 input = unpair(last);
					assert_product(output_type);
					Link negate = append11(UOP_NEGATE, input.l[0], child1(output_type));
					last = pair(negate, input.l[1], output_type);
					break;
				}
			case 'Q':
				{
					Link3 input = unpair2(last);
					assert_product(output_type);
					Type *number = child1(output_type);
					Link checked_number = append11(UOP_ASSERT_NONZERO, input.l[0], number);
					Link2 divmod = append22(UOP_DIVMOD,
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
					Link2 input = unpair(last);
					Link5 branches = unsum2(input.l[0]);
					assert_product(output_type);
					Type *type = child1(output_type);
					Link2 cond_inner = append12(UOP_COPY, branches.l[3],
					                               bool_type, bool_type);
					assert_sum(type);
					Link inner = sum(branches.l[0], branches.l[1], cond_inner.l[0],
					                        child1(type));
					Link cond_outer = or(cond_inner.l[1], branches.l[4]);
					Link outer = sum(inner, branches.l[2], cond_outer, type);
					last = pair(outer, input.l[1], output_type);
					break;
				}
			case 'R':
				{
					// Conditions:
					// a && b
					// a && !b
					Link2 input = unpair(last);
					Link3 outer = unsum(input.l[0]);
					Link3 inner = unsum(outer.l[0]);
					Link2 cond_outer = append12(UOP_COPY, outer.l[2],
					                                   bool_type, bool_type);
					Link2 cond_inner = append12(UOP_COPY, inner.l[2],
					                                   bool_type, bool_type);
					assert_product(output_type);
					Link new_outer = sum2(inner.l[0], inner.l[1], outer.l[1],
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
					Link2 input = unpair(last);
					Link5 branches = unsum2(input.l[0]);
					Link2 cond_outer = append12(UOP_COPY, branches.l[3],
					                                   bool_type, bool_type);
					assert_product(output_type);
					Link new_branches = sum2(branches.l[1], branches.l[0], branches.l[2],
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
					Link2 input = unpair(last);
					Link5 branches = unsum2(input.l[0]);
					Link3 inner = unsum(branches.l[2]);
					Link2 cond_outer = append12(UOP_COPY, branches.l[3],
					                                   bool_type, bool_type);
					Link2 not_cond_outer = append12(UOP_COPY, not(cond_outer.l[0]),
					                                       bool_type, bool_type);
					Link2 cond_middle = append12(UOP_COPY, branches.l[4],
					                                    bool_type, bool_type);
					assert_product(output_type);
					Type *type = child1(output_type);
					assert_sum(type);
					Type *type2 = child2(type);
					assert_sum(type2);
					Link new_inner = sum(branches.l[1], inner.l[1],
					                            and(not_cond_outer.l[0], cond_middle.l[0]),
					                            child2(type2));
					Link new_branches = sum2(branches.l[0], branches.l[2], new_inner,
					                                cond_outer.l[1],
					                                and(not_cond_outer.l[1],
					                                    and(not(cond_middle.l[1]), inner.l[2])),
					                                type);
					last = pair(new_branches, input.l[1], output_type);
					break;
				}
			case 'V':
				{
					Link2 input = unpair(last);
					assert_product(output_type);
					Type *type = child1(output_type);
					assert_sum(type);
					Link void_constant = append01(UOP_VOID_CONSTANT, child2(type));
					Link bool_constant = append01(UOP_BOOL_CONSTANT, bool_type);
					bool_constant.node->boolean = true;
					Link branches = sum(input.l[0], void_constant, bool_constant, type);
					last = pair(branches, input.l[1], output_type);
					break;
				}
			case 'C':
				{
					Link2 input = unpair(last);
					Link3 branches = unsum(input.l[0]);
					append10(UOP_DROP, branches.l[1]);
					append10(UOP_DROP, branches.l[2]);
					last = pair(branches.l[0], input.l[1], output_type);
					break;
				}

			case '?':
				{
					Link3 input = unpair2(last);
					Link checked_block = append11(UOP_ASSERT_DROPPABLE,
					                                     input.l[0], input.l[0].type);
					Link3 branches = unsum(input.l[1]);
					assert_product(output_type);
					Type *type = child1(output_type);
					assert_sum(type);
					Link applied_branch = append21(UOP_APPLY,
					                                      checked_block, branches.l[0],
					                                      child1(type));
					Link new_branches = sum(applied_branch, branches.l[1], branches.l[2],
					                               type);
					last = pair(new_branches, input.l[2], output_type);
					break;
				}
			case 'D':
				{
					Link3 input = unpair2(last);
					Link3 branches = unsum(input.l[1]);
					Link2 cond = append12(UOP_COPY, branches.l[2], bool_type, bool_type);
					Link2 distrib = append22(UOP_DISTRIB, input.l[0], cond.l[0],
					                                input.l[0].type, input.l[0].type);
					assert_product(output_type);
					Type *type = child1(output_type);
					assert_sum(type);
					Link left = pair(distrib.l[0], branches.l[0],
					                        child1(type));
					Link right = pair(distrib.l[1], branches.l[1],
								 child2(type));
					Link new_branches = sum(left, right, cond.l[1], type);
					last = pair(new_branches, input.l[2], output_type);
					break;
				}
			case 'F':
				{
					Link2 input = unpair(last);
					Link3 branches = unsum(input.l[0]);
					Link2 cond = append12(UOP_COPY, branches.l[2], bool_type, bool_type);
					Link2 left = unpair(branches.l[0]);
					Link2 right = unpair(branches.l[1]);
					assert_product(output_type);
					Type *type = child2(output_type);
					assert_product(type);
					Link new_left = sum(left.l[0], right.l[0], cond.l[0],
					                           child1(output_type));
					Link new_right = sum(left.l[1], right.l[1], cond.l[1],
					                           child1(type));
					last = pair2(new_left, new_right, input.l[1], output_type);
					break;
				}
			case 'M':
				{
					Link2 input = unpair(last);
					Link3 branches = unsum(input.l[0]);
					assert_product(output_type);
					Link merge = append31(UOP_MERGE, branches.l[0], branches.l[1],
					                             branches.l[2],
					                             child1(output_type));
					last = pair(merge, input.l[1], output_type);
					break;
				}
			case 'K':
				{
					Link2 input = unpair(last);
					Link3 branches = unsum(input.l[0]);
					append20(UOP_ASSERT_VOID, branches.l[0], branches.l[2]);
					last = pair(branches.l[1], input.l[1], output_type);
					break;
				}

			case '>':
				{
					Link3 input = unpair2(last);
					Node *greater = append_node20(graph, UOP_GREATER, input.l[1], input.l[0]);
					Link g0 = {greater, 0, input.l[0].type};
					Link g1 = {greater, 1, input.l[1].type};
					Link g2 = {greater, 2, input.l[1].type};
					Link g3 = {greater, 3, input.l[0].type};
					Link g4 = {greater, 4, bool_type};
					greater->out.next = alloc(&graph->out_link_pool, sizeof(OutLinkChunk));
					OUT0(greater).type = g0.type;
					OUT1(greater).type = g1.type;
					OUT2(greater).type = g2.type;
					OUT3(greater).type = g3.type;
					OUT4(greater).type = g4.type;
					greater->out_count = 5;
					assert_product(output_type);
					Type *type = child1(output_type);
					assert_sum(type);
					// We have truth on left in the graph IR
					Link right = pair(g0, g1, child1(type));
					Link left  = pair(g2, g3, child2(type));
					Link branches = sum(left, right, not(g4), type);
					last = pair(branches, input.l[2], output_type);
					break;
				}

			default:
				{
					assert(!"Received invalid opcode");
				}
		}
	}
	last.node->out.links[last.slot].node = &graph->output;
	last.node->out.links[last.slot].slot = 0;
	graph->output.uop = UOP_END;
	graph->output.in_count = 1;
	IN0(&graph->output).node = last.node;
	IN0(&graph->output).slot = last.slot;
}

internal
GraphState build_graphs(BlockPtrArray blocks, Type *bool_type, b32 optimise) {
	GraphState state = {};
	state.optimise = optimise;
	foreach (block, blocks) {
		build_graph(&state, *block, bool_type);
	}
	return state;
}
