#include "string.h"

#include <stdlib.h>
#include <stdio.h>

void print_string(struct string_rc *str) {
	fwrite(str->data, 1, str->size, stdout);
}

void string_rc_decref(struct string_rc *str) {
	if (--str->refcount == 0) {
		free(str);
	}
}
