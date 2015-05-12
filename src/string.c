#include "array.c"

struct string_rc {
	usize size;
	u32 refcount;
	// Intel optimisation guide recommends at least 16 byte alignment for arrays
	u8 pad[16 - sizeof(usize) - sizeof(u32)];
	u8 data[];
};

internal
void print_string(FILE *out, struct string_rc *str) {
	fwrite(str->data, 1, str->size, out);
}

internal
void string_rc_decref(struct string_rc *str) {
	if (--str->refcount == 0) {
		free(str);
	}
}
