#include <stdlib.h>
#include <getopt.h>

#include "type.h"
#include "parser.h"
#include "peephole.h"
#include "infer_types.h"
#include "build_graphs.h"

extern char *global_source;
extern b32 global_verbose;
char *global_source = NULL;
b32 global_verbose = false;

#define log(...) if (global_verbose) { fprintf(stderr, __VA_ARGS__); }

usize count_ops(struct block_ptr_array blocks) {
	usize ops = 0;
	foreach (block, blocks) {
		ops += (*block)->size;
	}
	return ops;
}

internal
char *help_text =
	"Usage: abcc [options] [input file]\n"
	"\n"
	"Options:\n"
	"  -h, --help       Display available options\n"
	"  -c, --compile    Run full compilation (default)\n"
	"  -t, --typecheck  Only run typechecker\n"
	"  -v, --verbose    Display verbose output\n";

internal
struct option long_options[] = {
	{ "help",      no_argument, NULL, 'h' },
	{ "compile",   no_argument, NULL, 'c' },
	{ "typecheck", no_argument, NULL, 't' },
	{ "verbose",   no_argument, NULL, 'v' },
};

int main(int argc, char **argv) {
	enum mode {
		MODE_COMPILE,
		MODE_TYPECHECK,
	} mode = MODE_COMPILE;
	while (true) {
		i32 option = getopt_long(argc, argv, "hctv", long_options, NULL);
		if (option == -1) {
			break;
		}
		switch (option) {
			case 'h':
				printf("%s", help_text);
				return 0;
			case 'c':
				mode = MODE_COMPILE;
				break;
			case 't':
				mode = MODE_TYPECHECK;
				break;
			case 'v':
				global_verbose = true;
				break;
			case '?':
				return 2;
			default:
				assert(!"getopt returned invalid value");
		}
	}
	FILE *input;
	if (optind == argc) {
		global_source = "<stdin>";
		input = stdin;
	} else if (optind == argc - 1) {
		global_source = argv[optind];
		input = fopen(argv[optind], "r");
		if (!input) {
			perror("abcc");
			return 2;
		}
	} else {
		fprintf(stderr, "Expecting at most a single file argument, got %d\n", argc - optind);
		return 2;
	}
	struct parse_result result = parse(input);
	foreach (error, result.errors) {
		print_parse_error(*error);
		array_free(&error->line);
	}
	array_free(&result.errors);
	if (!result.block) {
		printf("Parse failed.\n");
		return 1;
	}
	log("Parse succeeded. %zu blocks.\n", result.blocks.size);

	log("Total opcodes before simplify: %zu\n", count_ops(result.blocks));
	peephole_simplify(result.blocks);
	log("Total opcodes after simplify: %zu\n", count_ops(result.blocks));

	struct type_pool pool = {};
	if (!infer_types(result.blocks, &pool)) {
		fprintf(stderr, "Type inference failed\n");
		return 1;
	}
	if (mode == MODE_TYPECHECK) {
		struct block *block = result.blocks.data[result.blocks.size - 1];
		struct type_ptr_u64_map vars = {};
		print_type(block->types[0], &vars);
		printf("\n");
		print_type(block->types[block->size], &vars);
		printf("\n");
		map_free(&vars);
		goto cleanup_typecheck;
	}
	build_graphs(result.blocks);

cleanup_typecheck:
	foreach (chunk, pool.chunks) {
		free(*chunk);
	}
	array_free(&pool.chunks);
	foreach (block, result.blocks) {
		block_free(*block);
		free(*block);
	}
	array_free(&result.blocks);
	return 0;
}
