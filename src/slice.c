#include <stdlib.h>

#include "slice.h"

void slice_grow_(struct u8_slice *slice, usize size) {
	if (slice->cap == 0) {
		slice->cap = 64;
		slice->data = malloc(slice->cap * size);
	} else {
		// XXX This causes the buffer to crawl forward in memory. This
		// should be a big deal, but I can't get it to actually cause
		// problems, nor to do any better with smaller factors.
		slice->cap = slice->cap * 2;
		slice->data = realloc(slice->data, slice->cap * size);
	}
}

void slice_trim_(struct u8_slice *slice, usize size) {
	if (slice->cap) {
		slice->cap = slice->size;
		slice->data = realloc(slice->data, slice->cap * size);
	}
}

void *slice_bump_(struct u8_slice *slice, usize size) {
	if (slice->size == slice->cap) {
		slice_grow_(slice, size);
	}
	slice->size++;
	return &slice->data[(slice->size - 1) * size];
}

void slice_free(void *slice) {
	struct u8_slice *slice2 = slice;
	free(slice2->data);
}
