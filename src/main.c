#include "generate_c.c"

#include <sys/wait.h>
#include <getopt.h>

internal char *global_source = NULL;
internal b32 global_verbose = false;

#define log(...) if (global_verbose) { fprintf(stderr, __VA_ARGS__); }

usize count_ops(BlockPtrArray blocks) {
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
	"  -e, --executable  Output an executable (default)\n"
	"  -G, --graphviz    Output the dataflow graph in graphviz format\n"
	"  -C, --c           Output the generated C source\n"
	"  -T, --save-temps  Save temporary source files\n"
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
	{ "save-temps", no_argument, NULL, 'T' },
	{ "verbose",    no_argument, NULL, 'v' },
};

// Exit codes:
// 0 - Success
// 1 - Failure due to ABC
// 2 - Failure due to user error
// 3 - Failure due to unusual conditions (missing /tmp, etc)
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
	b32 save_temps = false;
	while (true) {
		i32 option = getopt_long(argc, argv, "hctO::eGCTv", long_options, NULL);
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
			case 'T':
				save_temps = true;
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
	ParseResult result = parse(input);
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

	TypePool pool = {};
	if (!infer_types(result.blocks, &pool)) {
		fprintf(stderr, "Type inference failed\n");
		return 1;
	}
	if (mode == MODE_TYPECHECK) {
		Block *block = result.blocks.data[result.blocks.size - 1];
		TypePtrU64Map vars = {};
		print_type(stdout, block->types[0], &vars);
		printf("\n");
		print_type(stdout, block->types[block->size], &vars);
		printf("\n");
		map_free(&vars);
		goto cleanup_typecheck;
	}
	ProgramGraph program = build_graphs(result.blocks, pool.boolean, optlevel >= 1);
	switch (output) {
		case OUTPUT_EXECUTABLE:
			{
#define DIE_IF(expr) if (expr) { perror("abcc"); return 3; }
				char dirname[] = "/tmp/abcc-XXXXXX";
				DIE_IF(!mkdtemp(dirname));
				char *oldwd = getcwd(NULL, 0);
				DIE_IF(!oldwd);
				DIE_IF(chdir(dirname));

				char *cname = "program.c";
				FILE *cfile = fopen(cname, "w");
				DIE_IF(!cfile);
				generate_c(cfile, &program);
				DIE_IF(fclose(cfile));

				char *llname = "rts-intrin.ll";
				FILE *llfile = fopen(llname, "w");
				DIE_IF(!llfile);
				fprintf(llfile, "%s", rts_intrin_ll);
				DIE_IF(fclose(llfile));

				pid_t pid = fork();
				DIE_IF(pid == -1);
				if (pid == 0) {
					execlp("clang",
					       "clang",
					       "-std=c11",
					       "-Wall",
					       "-ffreestanding",
					       "-nostdlib",
					       debug ? "-g" : "-O2",
					       "-msse4.1",
					       cname,
					       llname,
					       (char *)NULL);
					perror("abcc");
					return 1;
				}
				int status;
				DIE_IF(waitpid(pid, &status, 0) == -1);
				if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
					// Clang hopefully printed out whatever caused it to fail.
					return 1;
				}

				size_t oldwd_size = strlen(oldwd);
				char *new_exe = malloc(oldwd_size + 7);
				memcpy(new_exe, oldwd, oldwd_size);
				memcpy(new_exe + oldwd_size, "/a.out", 7);
				DIE_IF(rename("a.out", new_exe));

				if (save_temps) {
					fprintf(stderr, "Temporary files saved to %s\n", dirname);
				} else {
					DIE_IF(remove(cname));
					DIE_IF(remove(llname));
					DIE_IF(chdir(oldwd));
					DIE_IF(remove(dirname));
				}
				break;
			}
		case OUTPUT_GRAPHVIZ:
			generate_graphviz(&program);
			break;
		case OUTPUT_C:
			generate_c(stdout, &program);
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

