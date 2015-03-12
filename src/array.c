#include <stdlib.h>

#include "array.h"

void array_grow_(struct u8_array *array, usize size) {
	if (array->cap == 0) {
		array->cap = 64;
		array->data = malloc(array->cap * size);
	} else {
		// XXX This causes the buffer to crawl forward in memory. This
		// should be a big deal, but I can't get it to actually cause
		// problems, nor to do any better with smaller factors.
		array->cap = array->cap * 2;
		array->data = realloc(array->data, array->cap * size);
	}
}

void array_trim_(struct u8_array *array, usize size) {
	if (array->cap) {
		array->cap = array->size;
		array->data = realloc(array->data, array->cap * size);
	}
}

void *array_bump_(struct u8_array *array, usize size) {
	if (array->size == array->cap) {
		array_grow_(array, size);
	}
	array->size++;
	return &array->data[(array->size - 1) * size];
}

void array_free(void *array) {
	struct u8_array *array2 = array;
	free(array2->data);
}
