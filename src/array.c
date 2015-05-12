#include "abcc.h"

#define DEFINE_ARRAY(type, name) \
	struct name ## _array { \
		type *data; \
		usize size; \
		usize cap; \
	};

#define DEFINE_SLICE(type, name) \
	struct name ## _slice { \
		type *data; \
		usize size; \
	};

DEFINE_ARRAY(u8, u8);

#define array_grow(array) array_grow_((struct u8_array *) (array), sizeof((array)->data[0]))
internal
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

#define array_bump(array, num) array_bump_((struct u8_array *) (array), (num), sizeof((array)->data[0]))
internal
void *array_bump_(struct u8_array *array, usize num, usize size) {
	while (array->size + num - 1 >= array->cap) {
		array_grow_(array, size);
	}
	array->size += num;
	return &array->data[(array->size - 1) * size];
}

#define array_push(array, elem) ({ \
		typeof(array) _array = (array); \
		*((typeof(_array->data))array_bump(_array, 1)) = elem; \
	})

#define array_pop(array) ({ \
		typeof(array) _array = (array); \
		_array->data[_array->size-- - 1]; \
	})

#define foreach(var, array) \
	for (usize var ## _index = 0; var ## _index != (usize)-1; var ## _index = (usize)-1) \
		for (typeof((array).data) var; \
		     var ## _index < (array).size && (var = &(array).data[var ## _index], true); \
		     var ## _index++)

#define u8_array_push_cstring(buf, str) u8_array_push_many(buf, (u8 *)str, sizeof(str) - 1)
internal
void u8_array_push_many(struct u8_array *buf, u8 *str, usize len) {
	for (usize i = 0; i < len; i++) {
		array_push(buf, str[i]);
	}
}

#define array_trim(array) array_trim_((struct u8_array *) (array), sizeof((array)->data[0]))
internal
void array_trim_(struct u8_array *array, usize size) {
	if (array->cap) {
		array->cap = array->size;
		array->data = realloc(array->data, array->cap * size);
	}
}

internal
void array_free(void *array) {
	struct u8_array *array2 = array;
	free(array2->data);
}
