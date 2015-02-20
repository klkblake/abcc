#ifndef SLICE_H

#include "types.h"

#define DEFINE_SLICE(type, name) \
	struct name ## _slice { \
		type *data; \
		usize size; \
		usize cap; \
	};

DEFINE_SLICE(u8, u8);

void slice_grow_(struct u8_slice *slice, usize size);
void slice_trim_(struct u8_slice *slice, usize size);
void *slice_bump_(struct u8_slice *slice, usize size);

#define slice_grow(slice) slice_grow_((struct u8_slice *) (slice), sizeof((slice)->data[0]))
#define slice_trim(slice) slice_trim_((struct u8_slice *) (slice), sizeof((slice)->data[0]))
#define slice_bump(slice) slice_bump_((struct u8_slice *) (slice), sizeof((slice)->data[0]))

#define slice_snoc(slice, elem) ({ \
		typeof(slice) _slice = (slice); \
		*((typeof(_slice->data))slice_bump(_slice)) = elem; \
	})

void slice_free(void *slice);
void slice_clear(void *slice);

#define foreach(var, slice) \
	for (usize var ## _index = 0; var ## _index != (usize)-1; var ## _index = (usize)-1) \
		for (typeof((slice).data) var; \
		     var ## _index < (slice).size && (var = &(slice).data[var ## _index], true); \
		     var ## _index++)

#define SLICE_H
#endif
