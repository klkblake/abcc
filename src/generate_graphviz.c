#include "build_graphs.c"

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
	"or",
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

internal
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

internal
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

internal
void print_node_link(Node *from, u32 slot_count, u32 slot, TypePtrU64Map *vars) {
	OutLink *to = out_link(from, slot);
	if (does_implicit_copies(from->uop)) {
		printf("node_%p -> node_%p:%s [label=\"",
		       from, to->node, in_port(to->node->in_count, to->slot));
	} else {
		printf("node_%p:%s -> node_%p:%s [label=\"",
		       from, out_port(slot_count, slot), to->node, in_port(to->node->in_count, to->slot));
	}
	print_type(stdout, to->type, vars);
	printf("\"]\n");
}

internal
void print_node(Node *node, Graph *graph, u64 traversal, TypePtrU64Map *vars) {
	if (node->seen == traversal) {
		return;
	}
	node->seen = traversal;
	if (node->uop == UOP_BOOL_CONSTANT) {
		printf("node_%p [label=\"constant: %s\"]\n", node, node->boolean ? "true" : "false");
	} else if (node->uop == UOP_NUMBER_CONSTANT) {
		printf("node_%p [label=\"constant: %f\"]\n", node, node->number);
	} else if (node->uop == UOP_BLOCK_CONSTANT) {
		printf("node_%p [label=\"block constant\"]\n", node);
		printf("node_%p -> node_%p [style=dotted,weight=0]\n", node, &node->block->input);
	} else {
		printf("node_%p [label=\"%s\"]\n", node, uop_names[node->uop]);
	}
	for (u32 i = 0; i < node->out_count; i++) {
		print_node_link(node, node->out_count, i, vars);
		print_node(out_link(node, i)->node, graph, traversal, vars);
	}
}

internal
void print_graph(Graph *graph, b32 is_main, u64 traversal) {
	if (!is_main) {
		printf("subgraph cluster_%p {\n", graph);
	}
	TypePtrU64Map vars = {};
	print_node(&graph->input, graph, traversal, &vars);
	for (Node *node = graph->constants; node; node = node->next_constant) {
		printf("node_%p -> node_%p [style=dotted,weight=0]\n", &graph->input, node);
		print_node(node, graph, traversal, &vars);
	}
	map_free(&vars);
	if (!is_main) {
		printf("}\n");
	}
}

internal
void generate_graphviz(BlockPtrArray blocks) {
	printf("digraph {\n");
	u64 traversal = global_traversal++;
	foreach (block, blocks) {
		print_graph(&(*block)->graph, block_index == blocks.size - 1, traversal);
	}
	printf("}\n");
}
