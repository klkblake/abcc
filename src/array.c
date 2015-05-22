#include "abcc.h"

#define DEFINE_ARRAY(type, name) \
	typedef struct { \
		type *data; \
		usize size; \
		usize cap; \
	} name ## Array;

DEFINE_ARRAY(u8, U8);
DEFINE_ARRAY(u8 *, U8Ptr);
DEFINE_ARRAY(u32, U32);
DEFINE_ARRAY(usize, USize);

#define array_grow(array) array_grow_((U8Array *) (array), sizeof((array)->data[0]))
internal
void array_grow_(U8Array *array, usize size) {
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

#define array_bump(array, num) array_bump_((U8Array *) (array), (num), sizeof((array)->data[0]))
internal
void *array_bump_(U8Array *array, usize num, usize size) {
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

#define u8Array_push_cstring(buf, str) u8Array_push_many(buf, (u8 *)str, sizeof(str) - 1)
internal
void u8Array_push_many(U8Array *buf, u8 *str, usize len) {
	for (usize i = 0; i < len; i++) {
		array_push(buf, str[i]);
	}
}

#define array_remove(array, elem) ({ \
		typeof(array) _array = (array); \
		typeof(elem) _elem = (elem); \
		for (usize _index = 0; _index < _array->size; _index++) { \
			if (_array->data[_index] == _elem) { \
				_array->data[_index] = _array->data[_array->size - 1]; \
				_array->size--; \
				break; \
			} \
		} \
	})

#define array_trim(array) array_trim_((U8Array *) (array), sizeof((array)->data[0]))
internal
void array_trim_(U8Array *array, usize size) {
	if (array->cap) {
		array->cap = array->size;
		array->data = realloc(array->data, array->cap * size);
	}
}

internal
void array_free(void *array) {
	U8Array *array2 = array;
	free(array2->data);
}
