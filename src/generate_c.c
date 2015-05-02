#include "generate_c.h"

#include "type.h"

#include <stdlib.h>
#include <stdio.h>

extern char *rts_c;

void do_indent(u32 indent) {
	for (u32 i = 0; i < indent; i++) {
		putchar('\t');
	}
}

#define out(fmt, ...) do_indent(indent); printf(fmt "\n", ##__VA_ARGS__)

internal
void generate(struct graph *graph, u64 traversal1, u64 traversal2) {
	u32 indent = 1;
	printf("\nValue block_%p(Value v%u) {\n", graph, graph->input.out_link_id[0]);
	struct node_ptr_array worklist = {};
	array_push(&worklist, graph->input.out[0]);
	for (struct node *constant = graph->constants; constant; constant = constant->next_constant) {
		array_push(&worklist, constant);
	}
	graph->input.seen = traversal1;
	while (worklist.size > 0) {
		struct node *node = array_pop(&worklist);
		if (node->seen == traversal1) {
			continue;
		}
		b32 ready = true;
		for (u32 i = 0; i < node->in_count; i++) {
			if (node->in[i]->seen != traversal1) {
				ready = false;
			}
		}
		if (!ready) {
			continue;
		}
		node->seen = traversal1;
		for (u32 i = 0; i < node->out_count; i++) {
			out("Value v%u;", node->out_link_id[i]);
			array_push(&worklist, node->out[i]);
		}
	}
	struct node_ptr_array cond_stack = {};
	array_push(&worklist, graph->input.out[0]);
	for (struct node *constant = graph->constants; constant; constant = constant->next_constant) {
		array_push(&worklist, constant);
	}
	graph->input.seen = traversal2;
	while (worklist.size > 0) {
		struct node *node = array_pop(&worklist);
		if (node->seen == traversal2) {
			continue;
		}
		b32 ready = true;
		for (u32 i = 0; i < node->in_count; i++) {
			if (node->in[i]->seen != traversal2) {
				ready = false;
			}
		}
		if (!ready) {
			if (node->uop == UOP_SUM || node->uop == UOP_MERGE) {
				indent--;
				out("} else {");
				indent++;
			}
			continue;
		}
		node->seen = traversal2;
		u32 in[2];
		u32 out[4];
		for (u32 i = 0; i < node->in_count; i++) {
			in[i] = node->in[i]->out_link_id[node->src_slot[i]];
		}
		for (u32 i = 0; i < node->out_count; i++) {
			out[i] = node->out_link_id[i];
		}
		switch (node->uop) {
			case UOP_START: assert(!"Start node pushed onto worklist");
			case UOP_END:
				{
					out("return v%u;", in[0]);
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
					indent--;
					out("}");
					struct node *cond_node = array_pop(&cond_stack);
					out("v%u = alloc_sum(cond_%u, v%u);",
					    out[0], cond_node->out_link_id[0], in[0]);
					break;
				}
			case UOP_UNSUM:
				{
					array_push(&cond_stack, node);
					out("b32 cond_%u = v%u.sum & 0x1;", out[0], in[0]);
					out("v%u = (v%u.sum &~ 0x1)->value;", out[0], in[0]);
					out("v%u = v%u;", out[1], out[0]);
					out("if (cond_%u) {", out[0]);
					indent++;
					break;
				}
			case UOP_UNIT_CONSTANT:
				{
					out("v%u = UNIT;", out[0]);
					break;
				}
			case UOP_VOID_CONSTANT:
				{
					out("v%u = VOID;", out[0]);
					break;
				}
			case UOP_BLOCK_CONSTANT:
				{
					out("v%u = alloc_block_direct(&block_%p);", out[0], node->block);
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
					union type *in_type = node->in[0]->output_type[node->src_slot[0]];
					while (!IS_VAR(in_type) && IS_SEALED(in_type->symbol)) {
						// FIXME This can loop infinitely if there is a loop of
						// seals. This is invalid, but we don't check for it.
						in_type = deref(in_type->child1);
					}
					if (IS_VAR(in_type)) {
						out("XXX FAIL UNIMPLEMENTED COPY VAR XXX");
					} else {
						switch (in_type->symbol) {
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
							case SYMBOL_UNIT:
								{
									break;
								}
							case SYMBOL_NUMBER:
								{
									break;
								}
							default:
								out("XXX FAIL UNIMPLEMENTED COPY SYMBOL %lu XXX",
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
					out("v%u = alloc_block_composed(v%u, v%u);", out[0], in[0], in[1]);
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
					out("v%u = __builtin_floor(v%u/v%u);", out[1], in[1], in[0]);
					out("v%u = v%u - v%u * v%u;", out[0], in[1], out[1], in[0]);
					break;
				}
			case UOP_DISTRIB:
				{
					out("// Distrib");
					out("v%u = v%u;", out[0], in[0]);
					out("v%u = v%u;", out[1], in[0]);
					break;
				}
			case UOP_MERGE:
				{
					indent--;
					out("}");
					struct node *cond_node = array_pop(&cond_stack);
					out("v%u = cond_%u ? v%u : v%u;",
					    out[0], cond_node->out_link_id[0], in[0], in[1]);
					break;
				}
			case UOP_GREATER:
				{
					array_push(&cond_stack, node);
					out("b32 cond_%u = v%u > v%u;", out[0], in[0], in[1]);
					out("v%u = v%u;", out[0], in[1]);
					out("v%u = v%u;", out[1], in[0]);
					out("v%u = v%u;", out[2], in[0]);
					out("v%u = v%u;", out[3], in[1]);
					out("if (cond_%u) {", out[0]);
					indent++;
					break;
				}
			case UOP_ASSERT_COPYABLE:
			case UOP_ASSERT_DROPPABLE:
			case UOP_ASSERT_NONZERO:
			case UOP_ASSERT_VOID:
			case UOP_ASSERT_EQUAL:
			case UOP_DEBUG_PRINT_RAW:
			case UOP_DEBUG_PRINT_TEXT:
				;
		}
		for (u32 i = 0; i < node->out_count; i++) {
			array_push(&worklist, node->out[i]);
		}
	}
	printf("}\n");
	array_free(&worklist);
}

void generate_c(struct block_ptr_array blocks) {
	u64 traversal1 = global_traversal++;
	u64 traversal2 = global_traversal++;
	printf("%s", rts_c);
	foreach (block, blocks) {
		generate(&(*block)->graph, traversal1, traversal2);
	}
}
