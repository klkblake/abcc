#ifndef MAP_H

#include "abcc.h"

#define DEFINE_MAP_STRUCT(key_type, value_type, name) \
	struct name ## _map { \
		u32 *hashes; \
		key_type *keys; \
		value_type *values; \
		usize size; \
		usize num_buckets; \
	};

#define DEFINE_MAP_RESULT_STRUCT(value_type, name) \
	struct name ## _map_get_result { \
		value_type value; \
		usize bucket; \
		b32 found; \
	};

#define DEFINE_MAP_GROW(key_type, value_type, name) \
	void name ## _map_grow(struct name ## _map *map) { \
		usize num_buckets = map->num_buckets * 2; \
		if (num_buckets == 0) { \
			num_buckets = 64; \
		} \
		u32 *hashes        = malloc(num_buckets * sizeof(u32)); \
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

#define DEFINE_MAP_GET(key_type, value_type, name, hash_func) \
	struct name ## _map_get_result name ## _map_get(struct name ## _map *map, key_type key) { \
		if (map->num_buckets == 0) { \
			name ## _map_grow(map); \
		} \
		usize mask = map->num_buckets - 1; \
		u32 hash = hash_func(key); \
		if (hash == 0) { \
			hash = 0x80000000; \
		} \
		usize bucket = hash & mask; \
		while (map->hashes[bucket] != 0) { \
			if (map->hashes[bucket] == hash && map->keys[bucket] == key) { \
				return (struct name ## _map_get_result){map->values[bucket], bucket, true}; \
			} \
			bucket = (bucket + 1) & mask; \
		} \
		return (struct name ## _map_get_result){.bucket = bucket, .found = false}; \
	}

#define DEFINE_MAP_PUT_BUCKET(key_type, value_type, name, hash_func) \
	void name ## _map_put_bucket(struct name ## _map *map, key_type key, value_type value, usize bucket) { \
		u32 hash = hash_func(key); \
		if (hash == 0) { \
			hash = 0x80000000; \
		} \
		map->hashes[bucket] = hash; \
		map->keys[bucket] = key; \
		map->values[bucket] = value; \
		map->size++; \
		if (map->size > map->num_buckets / 2) { \
			name ## _map_grow(map); \
		} \
	}

#define DEFINE_MAP(key_type, value_type, name, hash_func) \
	DEFINE_MAP_STRUCT(key_type, value_type, name); \
	DEFINE_MAP_RESULT_STRUCT(value_type, name); \
	DEFINE_MAP_GROW(key_type, value_type, name); \
	DEFINE_MAP_GET(key_type, value_type, name, hash_func); \
	DEFINE_MAP_PUT_BUCKET(key_type, value_type, name, hash_func);

DEFINE_MAP_STRUCT(void *, void *, void_ptr);

void map_free_(struct void_ptr_map * map);
#define map_free(map) map_free_((struct void_ptr_map *) map)

#define MAP_H
#endif
