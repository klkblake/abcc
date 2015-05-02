#ifndef ARRAY_H

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

void array_grow_(struct u8_array *array, usize size);
void array_trim_(struct u8_array *array, usize size);
void *array_bump_(struct u8_array *array, usize num, usize size);

#define array_grow(array) array_grow_((struct u8_array *) (array), sizeof((array)->data[0]))
#define array_trim(array) array_trim_((struct u8_array *) (array), sizeof((array)->data[0]))
#define array_bump(array, num) array_bump_((struct u8_array *) (array), (num), sizeof((array)->data[0]))

#define array_push(array, elem) ({ \
		typeof(array) _array = (array); \
		*((typeof(_array->data))array_bump(_array, 1)) = elem; \
	})

#define array_pop(array) ({ \
		typeof(array) _array = (array); \
		_array->data[_array->size-- - 1]; \
	})

void array_free(void *array);

#define foreach(var, array) \
	for (usize var ## _index = 0; var ## _index != (usize)-1; var ## _index = (usize)-1) \
		for (typeof((array).data) var; \
		     var ## _index < (array).size && (var = &(array).data[var ## _index], true); \
		     var ## _index++)

void u8_array_push_many(struct u8_array *array, u8 *str, usize len);
#define u8_array_push_cstring(buf, str) u8_array_push_many(buf, (u8 *)str, sizeof(str) - 1)

#define ARRAY_H
#endif
