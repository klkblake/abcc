#include "map.h"

#include <stdlib.h>

void map_free_(struct void_ptr_map *map) {
	free(map->hashes);
	free(map->keys);
	free(map->values);
}