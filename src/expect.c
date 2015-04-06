#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "abcc.h"

void do_indent(u32 indent) {
	for (u32 i = 0; i < indent; i++) {
		putchar('\t');
	}
}

#define iprintf(fmt, ...) do_indent(indent); printf(fmt "\n", ##__VA_ARGS__)

char *construct(char *directive, u32 *var) {
	char c = *directive++;
	char *constructor;
	switch (c) {
		case 'v':
			printf("vars[%d] = var()", (*var)++);
			break;
		case '1':
			printf("pool->unit");
			break;
		case 'N':
			printf("pool->number");
			break;
		case '*':
		case '+':
		case '[':
			switch (c) {
				case '*':
					constructor = "prod";
					break;
				case '+':
					constructor = "sum";
					break;
				case '[':
					constructor = "block";
					break;
				default:
					assert(!"Impossible control flow");
			}
			printf("%s(", constructor);
			directive = construct(directive, var);
			printf(", ");
			directive = construct(directive, var);
			printf(")");
			break;
		default:
			fprintf(stderr, "expect: Unexpected directive char '%c'\n", *directive);
			exit(1);
	}
	return directive;
}

char *consume(char *directive, u32 *var, u32 indent, u32 loc) {
	char c = *directive++;
	char *sym;
	char *type;
	switch (c) {
		case 'v':
			iprintf("vars[%d] = loc%u;", (*var)++, loc);
			break;
		case '1':
		case 'N':
			switch (c) {
				case '1':
					sym = "UNIT";
					type = "unit";
					break;
				case 'N':
					sym = "NUMBER";
					type = "number";
					break;
				default:
					assert(!"Impossible control flow");
			}
			iprintf("if (IS_VAR(loc%u)) {", loc);
			iprintf("	assign(loc%u, pool->%s);", loc, type);
			iprintf("} else if (loc%u->symbol != SYMBOL_%s) {", loc, sym);
			iprintf("	fail_expect(loc%u);", loc);
			iprintf("}");
			break;
		case '*':
		case '+':
		case '[':
			switch (c) {
				case '*':
					sym = "PRODUCT";
					type = "product";
					break;
				case '+':
					sym = "SUM";
					type = "sum";
					break;
				case '[':
					sym = "BLOCK";
					type = "block";
					break;
				default:
					assert(!"Impossible control flow");
			}
			iprintf("if (IS_VAR(loc%u)) {", loc);
			do_indent(indent);
			printf("	assign_alloc(loc%u, SYMBOL_%s, ", loc, sym);
			u32 var2 = *var;
			char *construct_directive = construct(directive, &var2);
			printf(", ");
			construct(construct_directive, &var2);
			printf(", pool);\n");
			if (c == '[') {
				iprintf("} else if ((loc%u->symbol & POLYMORPHIC_MASK) == SYMBOL_%s) {", loc, sym);
			} else {
				iprintf("} else if (loc%u->symbol == SYMBOL_%s) {", loc, sym);
			}
			iprintf("	union type *loc%u = deref(loc%u->child1);", loc + 1, loc);
			directive = consume(directive, var, indent + 1, loc + 1);
			iprintf("	union type *loc%u = deref(loc%u->child2);", loc + 2, loc);
			directive = consume(directive, var, indent + 1, loc + 2);
			iprintf("} else {");
			iprintf("	fail_expect(loc%u);", loc);
			iprintf("}");
			break;
		default:
			fprintf(stderr, "expect: Unexpected directive char '%c'\n", *directive);
			exit(1);
	}
	return directive;
}

void expect(char *directive) {
	printf("{\n");
	printf("	union type *loc0 = deref(input);\n");
	u32 var = 0;
	directive = consume(directive, &var, 1, 0);
	char c = *directive;
	if (c != ' ' && c != '\n' && c != '\0') {
		fprintf(stderr, "expect: Extra character after directive: '%c'\n", c);
		exit(1);
	}
	printf("}\n");
}

void optype(char *directive, usize size) {
	char *end = memchr(directive, ' ', size);
	if (end == NULL) {
		fprintf(stderr, "expect: no output provided");
		exit(1);
	}
	end++;
	printf("output(");
	fwrite(end, 1, size - (usize) (end - directive) - 1, stdout);
	printf(");\n");
}

int main() {
	char *line = NULL;
	usize cap = 0;
	ssize_t size = 0;
	errno = 0;
	while ((size = getline(&line, &cap, stdin)) != -1) {
		char *comment = memmem(line, (usize) size, "//", 2);
		if (comment == NULL) {
			goto no_match;
		}
		char *directive = comment;
		usize directive_size = (usize) (size - (directive - line));
		directive += 2;
		directive_size -= 2;
		if (directive_size < 8) {
			goto no_match;
		}
		b32 do_expect = false;
		b32 do_output = false;
		if (memcmp(directive, "optype: ", 8) == 0) {
			do_expect = true;
			do_output = true;
		} else if (memcmp(directive, "expect: ", 8) == 0) {
			do_expect = true;
		}
		if (do_expect) {
			directive += 8;
			directive_size -= 8;
			fwrite(line, 1, (usize) (comment - line), stdout);
			putchar('\n');
			expect(directive);
			if (do_output) {
				optype(directive, directive_size);
			}
			putchar('\n');
			continue;
		}
no_match:
		fwrite(line, 1, (usize) size, stdout);
	}
	free(line);
	if (errno != 0) {
		perror("expect");
		return 1;
	}
	if (ferror(stdout)) {
		fprintf(stderr, "expect: could not write to stdout");
	}
	return 0;
}
