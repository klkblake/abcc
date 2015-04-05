#include "string.h"

#include <stdlib.h>

void print_string(FILE *out, struct string_rc *str) {
	fwrite(str->data, 1, str->size, out);
}

void string_rc_decref(struct string_rc *str) {
	if (--str->refcount == 0) {
		free(str);
	}
}
