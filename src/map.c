#include "string.c"

#define DEFINE_MAP_STRUCT(key_type, value_type, name) \
	typedef struct { \
		u32 *hashes; \
		key_type *keys; \
		value_type *values; \
		usize size; \
		usize num_buckets; \
	} name ## Map;

#define DEFINE_MAP_RESULT_STRUCT(value_type, name) \
	typedef struct { \
		value_type value; \
		usize bucket; \
		b32 found; \
	} name ## MapGetResult;

#define DEFINE_MAP_GROW(key_type, value_type, func_name, name) \
	void func_name ## _map_grow(name ## Map *map) { \
		usize num_buckets = map->num_buckets * 2; \
		if (num_buckets == 0) { \
			num_buckets = 64; \
		} \
		u32 *hashes        = calloc(sizeof(u32), num_buckets); \
		key_type *keys     = malloc(num_buckets * sizeof(key_type)); \
		value_type *values = malloc(num_buckets * sizeof(value_type)); \
		usize mask = num_buckets - 1; \
		for (usize i = 0; i < map->num_buckets; i++) { \
			u32 hash = map->hashes[i]; \
			if (hash == 0) { \
				continue; \
			} \
			usize bucket = hash & mask; \
			while (hashes[bucket] != 0) { \
				bucket = (bucket + 1) & mask; \
			} \
			hashes[bucket] = hash; \
			keys[bucket] = map->keys[i]; \
			values[bucket] = map->values[i]; \
		} \
		free(map->hashes); \
		free(map->keys); \
		free(map->values); \
		map->hashes = hashes; \
		map->keys = keys; \
		map->values = values; \
		map->num_buckets = num_buckets; \
	}

#define DEFINE_MAP_GET(key_type, value_type, func_name, name, hash_func) \
	name ## MapGetResult func_name ## _map_get(name ## Map *map, key_type key) { \
		if (map->num_buckets == 0) { \
			func_name ## _map_grow(map); \
		} \
		usize mask = map->num_buckets - 1; \
		u32 hash = hash_func(key); \
		if (hash == 0) { \
			hash = 0x80000000; \
		} \
		usize bucket = hash & mask; \
		while (map->hashes[bucket] != 0) { \
			if (map->hashes[bucket] == hash && map->keys[bucket] == key) { \
				return (name ## MapGetResult){map->values[bucket], bucket, true}; \
			} \
			bucket = (bucket + 1) & mask; \
		} \
		return (name ## MapGetResult){.bucket = bucket, .found = false}; \
	}

#define DEFINE_MAP_PUT_BUCKET(key_type, value_type, func_name, name, hash_func) \
	void func_name ## _map_put_bucket(name ## Map *map, key_type key, value_type value, usize bucket) { \
		u32 hash = hash_func(key); \
		if (hash == 0) { \
			hash = 0x80000000; \
		} \
		map->hashes[bucket] = hash; \
		map->keys[bucket] = key; \
		map->values[bucket] = value; \
		map->size++; \
		if (map->size > map->num_buckets / 2) { \
			func_name ## _map_grow(map); \
		} \
	}

#define DEFINE_MAP(key_type, value_type, func_name, name, hash_func) \
	DEFINE_MAP_STRUCT(key_type, value_type, name); \
	DEFINE_MAP_RESULT_STRUCT(value_type, name); \
	DEFINE_MAP_GROW(key_type, value_type, func_name, name); \
	DEFINE_MAP_GET(key_type, value_type, func_name, name, hash_func); \
	DEFINE_MAP_PUT_BUCKET(key_type, value_type, func_name, name, hash_func);

DEFINE_MAP_STRUCT(void *, void *, VoidPtr);

// We never shrink maps
#define map_delete_bucket(map, bucket) map_delete_bucket_((VoidPtrMap *) map, bucket)
internal
void map_delete_bucket_(VoidPtrMap *map, usize bucket) {
	map->hashes[bucket] = 0;
	map->size--;
}

#define map_free(map) map_free_((VoidPtrMap *) map)
internal
void map_free_(VoidPtrMap *map) {
	free(map->hashes);
	free(map->keys);
	free(map->values);
}
