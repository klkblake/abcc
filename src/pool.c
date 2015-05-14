#include "map.c"

#define CHUNK_SIZE 512

DEFINE_ARRAY(u8 *, U8Ptr);

typedef struct {
	U8PtrArray chunks;
	u32 used;
} Pool;

internal inline
void *alloc(Pool *pool, u32 size) {
	if (pool->used == CHUNK_SIZE || pool->chunks.size == 0) {
		array_push(&pool->chunks, malloc(CHUNK_SIZE * size));
		pool->used = 0;
	}
	void *result = &pool->chunks.data[pool->chunks.size - 1][pool->used++ * size];
	memset(result, 0, size);
	return result;
}
