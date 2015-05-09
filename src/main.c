#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

#include "type.h"
#include "parser.h"
#include "peephole.h"
#include "infer_types.h"
#include "build_graphs.h"
#include "generate_graphviz.h"
#include "generate_c.h"

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
	"  -h, --help        Display available options\n"
	"  -c, --compile     Run full compilation (default)\n"
	"  -t, --typecheck   Only run typechecker\n"
	"  -O0               Disable optimisation (not recommended)\n"
	"  -O1               Enable basic optimisations (default)\n"
	"  -O2, -O           Enable advanced optimisations\n"
	"  -g, --debug       Embed debugging information (prevents reproducible builds)\n"
	"  -e, --executable  Output an executable (default)"
	"  -G, --graphviz    Output the dataflow graph in graphviz format\n"
	"  -C, --c           Output the generated C source\n"
	"  -v, --verbose     Display verbose output\n";

internal
struct option long_options[] = {
	{ "help",       no_argument, NULL, 'h' },
	{ "compile",    no_argument, NULL, 'c' },
	{ "typecheck",  no_argument, NULL, 't' },
	{ "debug",      no_argument, NULL, 'g' },
	{ "executable", no_argument, NULL, 'e' },
	{ "graphviz",   no_argument, NULL, 'G' },
	{ "c",          no_argument, NULL, 'C' },
	{ "verbose",    no_argument, NULL, 'v' },
};

extern char *rts_intrin_ll;

int main(int argc, char **argv) {
	enum mode {
		MODE_COMPILE,
		MODE_TYPECHECK,
	} mode = MODE_COMPILE;
	enum output {
		OUTPUT_EXECUTABLE,
		OUTPUT_GRAPHVIZ,
		OUTPUT_C,
	} output = OUTPUT_EXECUTABLE;
	u32 optlevel = 1;
	b32 debug = false;
	while (true) {
		i32 option = getopt_long(argc, argv, "hctO::eGCv", long_options, NULL);
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
			case 'O':
				if (optarg == NULL || optarg[0] == '\0') {
					optlevel = 2;
				} else if (optarg[1] == '\0') {
					optlevel = (u32)optarg[0] - '0';
					if (optlevel > 2) {
						fprintf(stderr, "%s: -O requires an argument of 0, 1, or 2\n",
						        argv[0]);
						return 2;
					}
				} else {
					fprintf(stderr, "%s: -O requires an argument of 0, 1, or 2\n", argv[0]);
					return 2;
				}

				break;
			case 'g':
				debug = true;
				break;
			case 'e':
				output = OUTPUT_EXECUTABLE;
				break;
			case 'G':
				output = OUTPUT_GRAPHVIZ;
				break;
			case 'C':
				output = OUTPUT_C;
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
	build_graphs(result.blocks, pool.boolean, optlevel >= 1);
	switch (output) {
		case OUTPUT_EXECUTABLE:
			{
				char c_filename[] = "/tmp/abcc-c.XXXXXX.c";
				i32 fd = mkstemps(c_filename, 2);
				if (fd < 0) {
					perror("abcc");
					return 1;
				}
				FILE *c_file = fdopen(fd, "w");
				if (!c_file) {
					perror("abcc");
					return 1;
				}
				generate_c(c_file, result.blocks);
				if (fclose(c_file)) {
					perror("abcc");
					return 1;
				}
				char ll_filename[] = "/tmp/abcc-ll.XXXXXX.ll";
				fd = mkstemps(ll_filename, 3);
				if (fd < 0) {
					perror("abcc");
					return 1;
				}
				FILE *ll_file = fdopen(fd, "w");
				if (!ll_file) {
					perror("abcc");
					return 1;
				}
				fprintf(ll_file, "%s", rts_intrin_ll);
				if (fclose(ll_file)) {
					perror("abcc");
					return 1;
				}
				execlp("clang",
				       "clang",
				       "-std=c11",
				       "-Wall",
				       "-ffreestanding",
				       "-nostdlib",
				       debug ? "-g" : "-O2",
				       "-msse4.1",
				       ll_filename,
				       c_filename,
				       (char *)NULL);
				perror("abcc");
				return 1;
				// TODO delete temporary files
			}
		case OUTPUT_GRAPHVIZ:
			generate_graphviz(result.blocks);
			break;
		case OUTPUT_C:
			generate_c(stdout, result.blocks);
			break;
	}

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
