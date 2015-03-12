#include <stdlib.h>

#include "parser.h"
#include "peephole.h"
#include "infer_types.h"

usize count_ops(struct block_ptr_array blocks) {
	usize ops = 0;
	foreach (block, blocks) {
		ops += (*block)->size;
	}
	return ops;
}

int main() {
	struct parse_result result = parse(stdin);
	foreach (error, result.errors) {
		print_parse_error(*error);
		array_free(&error->line);
	}
	array_free(&result.errors);
	if (!result.block) {
		printf("Parse failed.\n");
		return 1;
	}
	printf("Parse succeeded. %zu blocks.\n", result.blocks.size);

	printf("Total opcodes before simplify: %zu\n", count_ops(result.blocks));
	peephole_simplify(result.blocks);
	printf("Total opcodes after simplify: %zu\n", count_ops(result.blocks));

	if (infer_types(result.blocks)) {
		printf("Type inference succeeded\n");
	} else {
		printf("Type inference failed\n");
	}

	foreach (block, result.blocks) {
		block_free(*block);
		free(*block);
	}
	array_free(&result.blocks);
	return 0;
}
