#include "generate_graphviz.h"

#include <stdio.h>

internal char *uop_names[] = {
	"START",
	"END",
	"seal",
	"unseal",
	"create pair",
	"destroy pair",
	"create sum",
	"destroy sum",
	"unit constant",
	"void constant",
	"block constant",
	"bool constant",
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
	"and",
	"not",
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

char *in_port(u32 slot_count, u32 slot) {
	static char *ports[] = {"nw", "n", "ne"};
	switch (slot_count) {
		case 1:
			return "c";
		case 2:
			return ports[slot * 2];
		case 3:
			return ports[slot];
		default:
			assert(!"Unhandled in count");
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
		case 5:
		        return ports[slot];
		default:
		        assert(!"Unhandled out count");
	}
}

void print_node_link(struct node *from, u32 slot_count, u32 slot) {
	struct node *to = from->out[slot];
	printf("node_%p:%s -> node_%p:%s\n",
	       from, out_port(slot_count, slot), to, in_port(to->in_count, from->dst_slot[slot]));
}

void print_node(struct node *node, struct graph *graph, u64 traversal) {
	if (node->seen == traversal) {
		return;
	}
	node->seen = traversal;
	if (node->uop == UOP_BOOL_CONSTANT) {
		printf("node_%p [label=\"constant: %s\"]\n", node, node->bool_value ? "true" : "false");
	} else if (node->uop == UOP_NUMBER_CONSTANT) {
		printf("node_%p [label=\"constant: %f\"]\n", node, node->number);
	} else if (node->uop == UOP_BLOCK_CONSTANT) {
		printf("node_%p [label=\"block constant\"]\n", node);
		printf("node_%p -> node_%p [style=dotted,constraint=false]\n", node, &node->block->input);
	} else {
		printf("node_%p [label=\"%s\"]\n", node, uop_names[node->uop]);
	}
	for (u32 i = 0; i < node->out_count; i++) {
		print_node_link(node, node->out_count, i);
		print_node(node->out[i], graph, traversal);
	}
}

void print_graph(struct graph *graph, b32 is_main, u64 traversal) {
	if (!is_main) {
		printf("subgraph cluster_%p {\n", graph);
	}
	print_node(&graph->input, graph, traversal);
	for (struct node *node = graph->constants; node; node = node->next_constant) {
		print_node(node, graph, traversal);
	}
	if (!is_main) {
		printf("}\n");
	}
}

void generate_graphviz(struct block_ptr_array blocks) {
	printf("digraph {\n");
	u64 traversal = global_traversal++;
	foreach (block, blocks) {
		print_graph(&(*block)->graph, block_index == blocks.size - 1, traversal);
	}
	printf("}\n");
}
