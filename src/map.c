#include "map.h"

#include <stdlib.h>

// We never shrink maps
void map_delete_bucket_(struct void_ptr_map *map, usize bucket) {
	map->hashes[bucket] = 0;
	map->size--;
}

void map_free_(struct void_ptr_map *map) {
	free(map->hashes);
	free(map->keys);
	free(map->values);
}
