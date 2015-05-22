#include "generate_graphviz.c"

internal
void do_indent(FILE *file, u32 indent) {
	for (u32 i = 0; i < indent; i++) {
		putc('\t', file);
	}
}

#define out(fmt, ...) do_indent(file, indent); fprintf(file, fmt "\n", ##__VA_ARGS__)

internal
void generate(FILE *file, BlockGraph *graph, b32 is_main, u32 *link_id, u64 traversal1, u64 traversal2) {
	u32 indent = 0;
	out("");
	out("static");
	OUT0(&graph->input).link_id = (*link_id)++;
	if (is_main) {
		out("Value block_main(Value v%u) {", OUT0(&graph->input).link_id);
	} else {
		out("Value block_%u(Value v%u) {", graph->id, OUT0(&graph->input).link_id);
	}
	indent++;
	NodePtrArray worklist = {};
	array_push(&worklist, OUT0(&graph->input).node);
	for (Node *constant = graph->constants; constant; constant = constant->next_constant) {
		array_push(&worklist, constant);
	}
	graph->input.seen = traversal1;
	while (worklist.size > 0) {
		Node *node = array_pop(&worklist);
		if (node->seen == traversal1) {
			continue;
		}
		b32 ready = true;
		for (u32 i = 0; i < node->in_count; i++) {
			if (in_link(node, i)->node->seen != traversal1) {
				ready = false;
			}
		}
		if (!ready) {
			continue;
		}
		node->seen = traversal1;
		for (u32 i = 0; i < node->out_count; i++) {
			OutLink *link = out_link(node, i);
			link->link_id = (*link_id)++;
			out("Value v%u;", link->link_id);
			array_push(&worklist, link->node);
		}
	}
	Node *const MARKER_ELSE = (Node *)0x1;
	Node *const MARKER_END  = (Node *)0x2;
	u32 return_value = (u32)-1;
	array_push(&worklist, OUT0(&graph->input).node);
	for (Node *constant = graph->constants; constant; constant = constant->next_constant) {
		array_push(&worklist, constant);
	}
	graph->input.seen = traversal2;
	while (worklist.size > 0) {
		Node *node = array_pop(&worklist);
		if (node == MARKER_ELSE) {
			indent--;
			out("} else {");
			indent++;
			continue;
		}
		if (node == MARKER_END) {
			indent--;
			out("}");
			continue;
		}
		if (node->seen == traversal2) {
			continue;
		}
		b32 ready = true;
		for (u32 i = 0; i < node->in_count; i++) {
			if (in_link(node, i)->node->seen != traversal2) {
				ready = false;
			}
		}
		if (!ready) {
			continue;
		}
		node->seen = traversal2;
		u32 in[node->in_count];
		u32 out[node->out_count];
		for (u32 i = 0; i < node->in_count; i++) {
			InLink *link = in_link(node, i);
			in[i] = out_link(link->node, link->slot)->link_id;
		}
		for (u32 i = 0; i < node->out_count; i++) {
			out[i] = out_link(node, i)->link_id;
		}
		b32 add_out_nodes = true;
		switch (node->uop) {
			case UOP_START: assert(!"Start node pushed onto worklist");
			case UOP_END:
				{
					return_value = in[0];
					break;
				}
			case UOP_SEAL:   out("v%u = v%u;", out[0], in[0]); break;
			case UOP_UNSEAL: out("v%u = v%u;", out[0], in[0]); break;
			case UOP_PAIR:
				{
					out("v%u = alloc_pair(v%u, v%u);", out[0], in[0], in[1]);
					break;
				}
			case UOP_UNPAIR:
				{
					out("v%u = v%u.pair->first;", out[0], in[0]);
					out("v%u = v%u.pair->second;", out[1], in[0]);
					out("decref(v%u);", in[0]);
					break;
				}
			case UOP_SUM:
				{
					out("v%u = alloc_sum(v%u.boolean, v%u.boolean ? v%u : v%u);",
					    out[0], in[2], in[2], in[0], in[1]);
					break;
				}
			case UOP_UNSUM:
				{
					out("v%u.boolean = v%u.bits & 0x1;", out[2], in[0]);
					out("v%u = ((Sum *)(v%u.bits &~ 0x1llu))->value;", out[0], in[0]);
					out("v%u = v%u;", out[1], out[0]);
					out("if (v%u.boolean) {", out[2]);
					indent++;
					array_push(&worklist, OUT2(node).node);
					array_push(&worklist, MARKER_END);
					array_push(&worklist, OUT1(node).node);
					array_push(&worklist, MARKER_ELSE);
					array_push(&worklist, OUT0(node).node);
					add_out_nodes = false;
					break;
				}
			case UOP_UNIT_CONSTANT:
				{
					out("v%u.bits = UNIT;", out[0]);
					break;
				}
			case UOP_VOID_CONSTANT:
				{
					out("v%u.bits = VOID;", out[0]);
					break;
				}
			case UOP_BLOCK_CONSTANT:
				{
					out("v%u = alloc_block_direct(&block_%u);", out[0], node->block->id);
					break;
				}
			case UOP_BOOL_CONSTANT:
				{
					out("v%u.boolean = %s;", out[0], node->boolean ? "true" : "false");
					break;
				}
			case UOP_NUMBER_CONSTANT:
				{
					out("v%u.number = %f;", out[0], node->number);
					break;
				}
			case UOP_TEXT_CONSTANT:
				{
					// TODO implement this
					out("XXX FAIL UNIMPLEMENTED TEXT CONSTANT XXX");
					break;
				}
			case UOP_COPY:
				{
					Type *in_type = out_link(IN0(node).node, IN0(node).slot)->type;
					while (!IS_VAR(in_type) && IS_SEALED(in_type->symbol)) {
						// FIXME This can loop infinitely if there is a loop of
						// seals. This is invalid, but we don't check for it.
						in_type = deref(in_type->child1);
					}
					if (IS_VAR(in_type)) {
						// TODO implement this
						out("XXX FAIL UNIMPLEMENTED COPY VAR XXX");
					} else {
						switch (in_type->symbol & POLYMORPHIC_MASK) {
							case SYMBOL_UNIT:   break;
							case SYMBOL_NUMBER: break;
							case SYMBOL_PRODUCT:
								{
									out("v%u.pair->refcount++;", in[0]);
									break;
								}
							case SYMBOL_SUM:
								{
									out("v%u.sum->refcount++;", in[0]);
									break;
								}
							case SYMBOL_BLOCK:
								{
									out("v%u.block->refcount++;", in[0]);
									break;
								}
							case SYMBOL_BOOL:   break;
							default:
							        // TODO implement this
								out("XXX FAIL UNIMPLEMENTED COPY SYMBOL %lx XXX",
								    in_type->symbol);
						}
					}
					out("v%u = v%u;", out[0], in[0]);
					out("v%u = v%u;", out[1], in[0]);
					break;
				}
			case UOP_DROP:
				{
					out("decref(v%u);", in[0]);
					break;
				}
			case UOP_APPLY:
				{
					out("v%u = apply(v%u.block, v%u);", out[0], in[0], in[1]);
					break;
				}
			case UOP_COMPOSE:
				{
					out("v%u = alloc_block_composed(v%u.block, v%u.block);", out[0], in[0], in[1]);
					break;
				}
			case UOP_QUOTE:
				{
					out("v%u = alloc_block_quote(v%u);", out[0], in[0]);
					break;
				}
			case UOP_MARK_RELEVANT: out("v%u = v%u;", out[0], in[0]); break;
			case UOP_MARK_AFFINE:   out("v%u = v%u;", out[0], in[0]); break;
			case UOP_ADD:
				{
					out("v%u.number = v%u.number + v%u.number;", out[0], in[0], in[1]);
					break;
				}
			case UOP_MULTIPLY:
				{
					out("v%u.number = v%u.number * v%u.number;", out[0], in[0], in[1]);
					break;
				}
			case UOP_INVERSE:
				{
					out("v%u.number = 1 / v%u.number;", out[0], in[0]);
					break;
				}
			case UOP_NEGATE:
				{
					out("v%u.number = -v%u.number;", out[0], in[0]);
					break;
				}
			case UOP_DIVMOD:
				{
					out("v%u.number = floor(v%u.number/v%u.number);",
					    out[1], in[1], in[0]);
					out("v%u.number = v%u.number - v%u.number * v%u.number;",
					    out[0], in[1], out[1], in[0]);
					break;
				}
			case UOP_AND:
				{
					out("v%u.boolean = v%u.boolean && v%u.boolean;", out[0], in[0], in[1]);
					for (u32 i = 1; i < node->out_count; i++) {
						out("v%u.boolean = v%u.boolean;", out[i], out[0]);
					}
					break;
				}
			case UOP_OR:
				{
					out("v%u.boolean = v%u.boolean || v%u.boolean;", out[0], in[0], in[1]);
					for (u32 i = 1; i < node->out_count; i++) {
						out("v%u.boolean = v%u.boolean;", out[i], out[0]);
					}
					break;
				}
			case UOP_NOT:
				{
					out("v%u.boolean = !v%u.boolean;", out[0], in[0]);
					for (u32 i = 1; i < node->out_count; i++) {
						out("v%u.boolean = v%u.boolean;", out[i], out[0]);
					}
					break;
				}
			case UOP_DISTRIB:
				{
					out("// Distribute");
					out("(void) v%u;", in[1]);
					out("v%u = v%u;", out[0], in[0]);
					out("v%u = v%u;", out[1], in[0]);
					break;
				}
			case UOP_MERGE:
				{
					out("v%u = v%u.boolean ? v%u : v%u;",
					    out[0], in[2], in[0], in[1]);
					break;
				}
			case UOP_GREATER:
				{
					out("v%u.boolean = v%u.number > v%u.number;", out[4], in[0], in[1]);
					out("v%u = v%u;", out[0], in[1]);
					out("v%u = v%u;", out[1], in[0]);
					out("v%u = v%u;", out[2], in[0]);
					out("v%u = v%u;", out[3], in[1]);
					out("if (v%u.boolean) {", out[4]);
					indent++;
					array_push(&worklist, OUT4(node).node);
					array_push(&worklist, MARKER_END);
					array_push(&worklist, OUT3(node).node);
					array_push(&worklist, OUT2(node).node);
					array_push(&worklist, MARKER_ELSE);
					array_push(&worklist, OUT1(node).node);
					array_push(&worklist, OUT0(node).node);
					add_out_nodes = false;
					break;
				}
			case UOP_ASSERT_COPYABLE:
				{
					out("v%u = v%u;", out[0], in[0]);
					break;
				}
			case UOP_ASSERT_DROPPABLE:
				{
					out("v%u = v%u;", out[0], in[0]);
					break;
				}
			case UOP_ASSERT_NONZERO:
				{
					out("assert_nonzero(v%u.number);", in[0]);
					out("v%u = v%u;", out[0], in[0]);
					break;
				}
			case UOP_ASSERT_VOID:
				{
					out("assert_void(v%u, v%u.boolean);", in[0], in[1]);
					break;
				}
			case UOP_ASSERT_EQUAL:
				{
					out("assert_equal(v%u, v%u);", in[0], in[1]);
					out("v%u = v%u;", out[0], in[0]);
					out("v%u = v%u;", out[1], in[1]);
					break;
				}
			case UOP_DEBUG_PRINT_RAW:
				{
					// TODO implement this
					out("XXX FAIL UNIMPLEMENTED PRINT RAW XXX");
					break;
				}
			case UOP_DEBUG_PRINT_TEXT:
				{
					// TODO implement this
					out("XXX FAIL UNIMPLEMENTED PRINT TEXT XXX");
					break;
				}
		}
		if (add_out_nodes) {
			for (u32 i = 0; i < node->out_count; i++) {
				array_push(&worklist, out_link(node, i)->node);
			}
		}
	}
	assert(return_value != (u32)-1);
	out("return v%u;", return_value);
	indent--;
	out("}");
	array_free(&worklist);
}

internal
void generate_c(FILE *file, ProgramGraph *program) {
	u64 traversal1 = global_traversal++;
	u64 traversal2 = global_traversal++;
	u32 link_id = 0;
	fprintf(file, "%s", rts_c);
	BlockGraphPtrArray blocks = flatten_dependencies(program->main);
	foreach (block, blocks) {
		generate(file, *block, *block == program->main, &link_id, traversal1, traversal2);
	}
}
