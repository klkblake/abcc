#include "array.c"

typedef struct {
	usize size;
	u32 refcount;
	// Intel optimisation guide recommends at least 16 byte alignment for arrays
	u8 pad[16 - sizeof(usize) - sizeof(u32)];
	u8 data[];
} StringRC;
DEFINE_ARRAY(StringRC *, StringRCPtr);

internal
void print_string(FILE *out, StringRC *str) {
	fwrite(str->data, 1, str->size, out);
}

internal
void string_rc_decref(StringRC *str) {
	if (--str->refcount == 0) {
		free(str);
	}
}
