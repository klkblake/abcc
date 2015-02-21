#include "peephole.h"

void simplify(struct block *block) {
}

void peephole_simplify(struct block_ptr_slice blocks) {
	foreach (block, blocks) {
		simplify(*block);
	}
}
