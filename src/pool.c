#include "map.c"

#define CHUNK_SIZE 512

typedef struct {
	U8PtrArray chunks;
	u32 used;
	void **first_free;
} Pool;

internal inline
void *alloc(Pool *pool, u32 size) {
	void *result = pool->first_free;
	if (result) {
		pool->first_free = *pool->first_free;
		memset(result, 0, size);
		return result;
	}
	if (pool->used == CHUNK_SIZE || pool->chunks.size == 0) {
		array_push(&pool->chunks, malloc(CHUNK_SIZE * size));
		pool->used = 0;
	}
	result = &pool->chunks.data[pool->chunks.size - 1][pool->used++ * size];
	memset(result, 0, size);
	return result;
}

internal inline
void release(Pool *pool, void *ptr) {
	void **entry = ptr;
	*entry = pool->first_free;
	pool->first_free = entry;
}

internal inline
void pool_free(Pool *pool) {
	foreach (chunk, pool->chunks) {
		free(*chunk);
	}
}
