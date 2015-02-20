#include "parser.h"

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

	foreach (block, result.blocks) {
		block_free(*block);
	}
	slice_free(&result.blocks);
	return 0;
}
