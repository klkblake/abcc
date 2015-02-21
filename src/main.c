#include "parser.h"
#include "peephole.h"

usize count_ops(struct block_ptr_slice blocks) {
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
	}
	slice_free(&result.errors);
	if (!result.block) {
		printf("Parse failed.\n");
		return 1;
	}
	printf("Parse succeeded. %zu blocks.\n", result.blocks.size);

	printf("Total opcodes before simplify: %zu\n", count_ops(result.blocks));
	peephole_simplify(result.blocks);
	printf("Total opcodes after simplify: %zu\n", count_ops(result.blocks));

	foreach (block, result.blocks) {
		block_free(*block);
	}
	slice_free(&result.blocks);
	return 0;
}
