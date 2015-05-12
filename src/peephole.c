#include "parser.c"

/*
 * This pass implements the following rewrite rules, matching in order:
 *
 * R3RIGHT = rz^QF
 * R3LEFT  = lz$o+*Q?D>
 *
 *         FROM        TO
 *         lr          ww
 *         lwwr        zz
 *         www         w
 * <R3RIGHT>ww         ?
 *         ww<R3LEFT>  ?
 *         wlzrw       lzr
 *         zwz         wzw
 *         wzz         zzw
 *         zzz         z
 */

#define MATCH2(s, c0, c1)             ((s & 0xffff)       ==                                                 (((c0) << 8) | (c1)))
#define MATCH3(s, c0, c1, c2)         ((s & 0xffffff)     ==                                  (((c0) << 16) | ((c1) << 8) | (c2)))
#define MATCH4(s, c0, c1, c2, c3)     ((s & 0xffffffff)   ==                   (((c0) << 24) | ((c1) << 16) | ((c2) << 8) | (c3)))
#define MATCH5(s, c0, c1, c2, c3, c4) ((s & 0xffffffffff) == (((u64)c0 << 32) | ((c1) << 24) | ((c2) << 16) | ((c3) << 8) | (c4)))

internal
void simplify(struct block *block) {
	usize new_size = block->size;
	u8 *ops = block->opcodes;
	u64 seen = 0;
	usize offsets[5] = {};

	for (usize i = 0; i < block->size; i++) {
		u8 op = ops[i];
		if (op == 0xff || op == OP_FRAME_PUSH || op == OP_FRAME_POP) {
			continue;
		}
		seen = ((seen << 8) | op);
		offsets[4] = offsets[3];
		offsets[3] = offsets[2];
		offsets[2] = offsets[1];
		offsets[1] = offsets[0];
		offsets[0] = i;
// TODO convert to function
#define backtrack(match, n) \
		do { \
			usize num = n; \
			seen = 0;\
			i = offsets[match - 1]; \
			i = i > 0 ? i - 1 : 0; \
			for (; num > 0 && i > 0; i--) { \
				op = ops[i]; \
				if (op == 0xff || op == OP_FRAME_PUSH || op == OP_FRAME_POP) { \
					continue; \
				} \
				if (--num == 0) { \
					break; \
				} \
			} \
			i--; \
		} while (false)
		if (MATCH2(seen, 'l', 'r')) {
			ops[offsets[0]] = 'w';
			ops[offsets[1]] = 'w';
			backtrack(2, 4);
			continue;
		}
		if (MATCH4(seen, 'l', 'w', 'w', 'r')) {
			ops[offsets[0]] = 0xff;
			ops[offsets[3]] = 0xff;
			new_size -= 2;
			backtrack(4, 2);
			continue;
		}
		if (MATCH3(seen, 'w', 'w', 'w')) {
			ops[offsets[0]] = 0xff;
			ops[offsets[2]] = 0xff;
			new_size -= 2;
			backtrack(3, 4);
			continue;
		}
		if (MATCH3(seen, 'r', 'w', 'w') ||
		    MATCH3(seen, 'z', 'w', 'w') ||
		    MATCH3(seen, '^', 'w', 'w') ||
		    MATCH3(seen, 'Q', 'w', 'w') ||
		    MATCH3(seen, 'F', 'w', 'w')) {
			ops[offsets[0]] = 0xff;
			ops[offsets[1]] = 0xff;
			new_size -= 2;
			backtrack(3, 3);
			continue;
		}
		if (MATCH3(seen, 'w', 'w', 'l') ||
		    MATCH3(seen, 'w', 'w', 'z') ||
		    MATCH3(seen, 'w', 'w', '$') ||
		    MATCH3(seen, 'w', 'w', 'o') ||
		    MATCH3(seen, 'w', 'w', '+') ||
		    MATCH3(seen, 'w', 'w', '*') ||
		    MATCH3(seen, 'w', 'w', 'Q') ||
		    MATCH3(seen, 'w', 'w', '?') ||
		    MATCH3(seen, 'w', 'w', 'D') ||
		    MATCH3(seen, 'w', 'w', '>')) {
			ops[offsets[1]] = 0xff;
			ops[offsets[2]] = 0xff;
			new_size -= 2;
			backtrack(3, 2);
			continue;
		}
		if (MATCH5(seen, 'w', 'l', 'z', 'r', 'w')) {
			ops[offsets[0]] = 0xff;
			ops[offsets[4]] = 0xff;
			new_size -= 2;
			backtrack(5, 2);
			continue;
		}
		if (MATCH3(seen, 'z', 'w', 'z')) {
			ops[offsets[0]] = 'w';
			ops[offsets[1]] = 'z';
			ops[offsets[2]] = 'w';
			backtrack(3, 4);
			continue;
		}
		if (MATCH3(seen, 'w', 'z', 'z')) {
			ops[offsets[0]] = 'w';
			ops[offsets[1]] = 'z';
			ops[offsets[2]] = 'z';
			backtrack(3, 2);
			continue;
		}
		if (MATCH3(seen, 'z', 'z', 'z')) {
			ops[offsets[0]] = 0xff;
			ops[offsets[2]] = 0xff;
			new_size -= 2;
			backtrack(3, 2);
			continue;
		}
#undef backtrack
	}
	u8 *new_ops = malloc(new_size);
	for (usize i = 0, j = 0; i < block->size; i++) {
		u8 op = ops[i];
		if (op == 0xff) {
			continue;
		}
		new_ops[j++] = ops[i];
	}
	free(ops);
	block->opcodes = new_ops;
	block->size = new_size;
}

internal
void peephole_simplify(struct block_ptr_array blocks) {
	foreach (block, blocks) {
		simplify(*block);
	}
}
