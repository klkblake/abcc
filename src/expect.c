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

char *construct(char *directive, u32 indent, u32 loc) {
	char c = *directive++;
	char *constructor;
	switch (c) {
		case 'v':
			iprintf("*loc%d = var();", loc);
			break;
		case '1':
			iprintf("*loc%d = types->unit;", loc);
			break;
		case 'N':
			iprintf("*loc%d = types->number;", loc);
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
			}
			iprintf("{");
			iprintf("	union type **loc%d = &(*loc%d)->child1;", loc + 1, loc);
			directive = construct(directive, indent + 1, loc + 1);
			iprintf("	union type **loc%d = &(*loc%d)->child2;", loc + 2, loc);
			directive = construct(directive, indent + 1, loc + 2);
			iprintf("	*loc%d = %s(*loc%d, *loc%d);", loc, constructor, loc + 1, loc + 2);
			iprintf("}");
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
			iprintf("vars[%d] = *loc%d;", (*var)++, loc);
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
			}
			iprintf("if (*loc%d == NULL) {", loc);
			iprintf("	*loc%d = types->%s;", loc, type);
			iprintf("} else if (IS_VAR(*loc%d)) {", loc);
			iprintf("	**loc%d = *types->%s;", loc, type);
			iprintf("	*loc%d = types->%s;", loc, type);
			iprintf("} else if ((*loc%d)->symbol != SYMBOL_%s){", loc, sym);
			iprintf("	// TODO report error");
			iprintf("	return false;");
			iprintf("}");
			break;
		case '*':
		case '+':
		case '[':
			switch (c) {
				case '*':
					sym = "PRODUCT";
					break;
				case '+':
					sym = "SUM";
					break;
				case '[':
					sym = "BLOCK";
					break;
			}
			iprintf("construct = false;");
			iprintf("if (*loc%d == NULL) {", loc);
			iprintf("	*loc%d = alloc_type(types);", loc);
			iprintf("	construct = true;");
			iprintf("}");
			iprintf("if (IS_VAR(*loc%d) || construct) {", loc);
			iprintf("	union type **loc%d = &(*loc%d)->child1;", loc + 1, loc);
			char *construct_directive = construct(directive, indent + 1, loc + 1);
			iprintf("	union type **loc%d = &(*loc%d)->child2;", loc + 2, loc);
			construct(construct_directive, indent + 1, loc + 2);
			iprintf("	(*loc%d)->symbol = SYMBOL_%s;", loc, sym);
			iprintf("	(*loc%d)->next = *loc%d;", loc, loc);
			iprintf("} else if ((*loc%d)->symbol == SYMBOL_%s){", loc, sym);
			iprintf("	union type **loc%d = &(*loc%d)->child1;", loc + 1, loc);
			directive = consume(directive, var, indent + 1, loc + 1);
			iprintf("	union type **loc%d = &(*loc%d)->child2;", loc + 2, loc);
			directive = consume(directive, var, indent + 1, loc + 2);
			iprintf("} else {");
			iprintf("	// TODO report error");
			iprintf("	return false;");
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
	printf("\tb32 construct;\n");
	printf("\tunion type **loc0 = &input;\n");
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
	fwrite(end, 1, size - (end - directive) - 1, stdout);
	printf(");\n");
}

int main() {
	char *line = NULL;
	usize cap = 0;
	ssize_t size = 0;
	errno = 0;
	while ((size = getline(&line, &cap, stdin)) != -1) {
		char *comment = memmem(line, size, "//", 2);
		if (comment == NULL) {
			goto no_match;
		}
		char *directive = comment;
		usize directive_size = size - (directive - line);
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
			fwrite(line, 1, comment - line, stdout);
			putchar('\n');
			expect(directive);
			if (do_output) {
				optype(directive, directive_size);
			}
			putchar('\n');
			continue;
		}
no_match:
		fwrite(line, 1, size, stdout);
	}
	if (errno != 0) {
		perror("expect");
		return 1;
	}
	if (ferror(stdout)) {
		fprintf(stderr, "expect: could not write to stdout");
	}
	return 0;
}