#include "string.h"

#include <stdlib.h>

void string_rc_decref(struct string_rc *str) {
	if (--str->refcount == 0) {
		free(str);
	}
}
