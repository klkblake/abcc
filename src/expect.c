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
			printf("types->unit");
			break;
		case 'N':
			printf("types->number");
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

char *consume(char *directive, u32 *var, u32 indent, char *loc, u32 varid) {
	char c = *directive++;
	char *sym;
	char *type;
	switch (c) {
		case 'v':
			iprintf("vars[%d] = %s;", (*var)++, loc);
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
			iprintf("if (%s == NULL) {", loc);
			iprintf("	%s = types->%s;", loc, type);
			iprintf("} else if (IS_VAR(%s)) {", loc);
			iprintf("	*%s = *types->%s;", loc, type);
			iprintf("	%s = types->%s;", loc, type);
			iprintf("} else if (%s->symbol != SYMBOL_%s){", loc, sym);
			iprintf("	printf(\"Error on opcode %%lu (%%c)\\n\", i, op);");
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
			if (loc > 0) {
				iprintf("b32 construct%d = false;", varid);
				iprintf("if (%s == NULL) {", loc);
				iprintf("	%s = alloc_type(types);", loc);
				iprintf("	construct%d = true;", varid);
				iprintf("}");
				iprintf("if (construct%d || IS_VAR(%s)) {", varid, loc);
			} else {
				iprintf("if (IS_VAR(%s)) {", loc);
			}
			iprintf("	%s->symbol = SYMBOL_%s;", loc, sym);
			iprintf("	%s->next = %s;", loc, loc);
			do_indent(indent);
			printf("	%s->child1 = ", loc);
			u32 var2 = *var;
			char *construct_directive = construct(directive, &var2);
			printf(";\n");
			do_indent(indent);
			printf("	%s->child2 = ", loc);
			construct(construct_directive, &var2);
			printf(";\n");
			iprintf("} else if (%s->symbol == SYMBOL_%s){", loc, sym);
			iprintf("	union type *loc%d = %s;", varid, loc);
			usize len = 3 /* loc */ + 10 /* varid */ + 8 /* ->childN */ + 1 /* NUL */;
			char *loc1 = alloca(len);
			char *loc2 = alloca(len);
			sprintf(loc1, "loc%d->child1", varid);
			sprintf(loc2, "loc%d->child2", varid);
			directive = consume(directive, var, indent + 1, loc1, varid + 1);
			directive = consume(directive, var, indent + 1, loc2, varid + 2);
			iprintf("} else {");
			iprintf("	printf(\"Error on opcode %%lu (%%c)\\n\", i, op);");
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
	u32 var = 0;
	directive = consume(directive, &var, 1, "input", 0);
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
