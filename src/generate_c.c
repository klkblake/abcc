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
	printf("Value block_%p(Value v%u) {\n", graph, graph->input.out_link_id[0]);
	struct node_ptr_array worklist = {};
	array_push(&worklist, graph->input.out[0]);
	for (struct node *constant = graph->constants; constant; constant = constant->next_constant) {
		array_push(&worklist, constant);
	}
	while (worklist.size > 0) {
		struct node *node = array_pop(&worklist);
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
	while (worklist.size > 0) {
		struct node *node = array_pop(&worklist);
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
			case UOP_SEAL:   break;
			case UOP_UNSEAL: break;
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
					out("__builtin_unreachable();");
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
					out("XXX FAIL UNIMPLEMENTED XXX");
					break;
				}
			case UOP_COPY:
				{
					union type *in_type = node->in[0]->output_type[node->src_slot[0]];
					assert(!IS_VAR(in_type));
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
							assert(!"Don't know how to do copy");
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
			case UOP_COMPOSE:
			case UOP_QUOTE:
			case UOP_MARK_RELEVANT:
			case UOP_MARK_AFFINE:
			case UOP_ADD:
			case UOP_MULTIPLY:
			case UOP_INVERSE:
			case UOP_NEGATE:
			case UOP_DIVMOD:
			case UOP_DISTRIB:
			case UOP_MERGE:
			case UOP_GREATER:
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
