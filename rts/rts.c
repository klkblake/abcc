#include "syscalls.h"
#include "rts.h"

#define DIE(name) do { write(2, name, sizeof(name)); exit(1); } while (0)
char out_of_mem[] = "Out of memory\n";

void *base = 0;
void *limit = 0;

void init_mm(void) {
	base = brk(0);
	limit = base;
}

void *malloc(long size) {
	if (base == limit) {
		limit = brk(limit + 16*4096);
		if (limit == base) {
			DIE(out_of_mem);
		}
	}
	void *result = base;
	base += size;
	return result;
}

Pair pair(Any a, Any b) {
	const void **result = malloc(sizeof(struct pair));
	result[0] = a;
	result[1] = b;
	return (Pair) result;
}

CompBlock compBlock(Any a, Any b) {
	const void **result = malloc(sizeof(struct comp_block));
	result[0] = a;
	result[1] = b;
	return (CompBlock) ((long) result | BLOCK_COMP);
}

#define f(v) (((Pair)(v))->fst)
#define s(v) (((Pair)(v))->snd)

#define ff(v) f(f(v))
#define fs(v) s(f(v))
#define sf(v) f(s(v))
#define ss(v) s(s(v))

#define ssf(v) f(s(s(v)))
#define sss(v) s(s(s(v)))

#define l0(v) f(v)
#define l1(v) sf(v)
#define l2(v) ssf(v)

#define t1(v) s(v)
#define t2(v) ss(v)
#define t3(v) sss(v)

#define v0 l0(v)
#define v1 l1(v)
#define v2 l2(v)

#define vt1 t1(v)
#define vt2 t2(v)
#define vt3 t3(v)

#define list1(a, l) pair(a, l)
#define list2(a, b, l) pair(a, pair(b, l))
#define list3(a, b, c, l) pair(a, pair(b, pair(c, l)))

OP(assocl) {
	return pair(pair(f(v), sf(v)), ss(v));
}

OP(assocr) {
	return list2(ff(v), fs(v), s(v));
}

OP(swap) {
	return list2(v1, v0, vt2);
}

OP(swapd) {
	return list3(v0, v2, v1, vt3);
}

OP(intro1) {
	return pair(v, Unit);
}

OP(elim1) {
	return v0;
}

OP(drop) {
	return v1;
}

OP(copy) {
	return list2(v0, v0, vt1);
}

Any applyBlock(Any b, Any x) {
	long ptr = (long) b;
	long type = ptr & 0x3;
	ptr &= ~0x3;
	CompBlock cb;
	switch (type) {
		case BLOCK_NORMAL:
		x = ((Block) ptr)(x);
		break;

		case BLOCK_COMP:
		cb = (CompBlock) ptr;
		x = applyBlock(cb->xy, x);
		x = applyBlock(cb->yz, x);
		break;

		case BLOCK_QUOTE:
		x = pair(*((Any *) ptr), x);
		break;
	}
	return x;
}

OP(apply) {
	return list1(applyBlock(v0, v1), vt2);
}

OP(compose) {
	return list1(compBlock(v1, v0), vt2);
}

OP(add) {
	return list1((Any) ((long)v0 + (long) v1), vt2);
}

OP(multiply) {
	return list1((Any) ((long)v0 * (long) v1), vt2);
}

#define COUNT 1000

extern Any block_0(Any);

int main(void) {
	init_mm();
	long out[COUNT];
	for (long i = 0; i < COUNT; i++) {
		Any v = pair(pair((void *) i, Unit), Unit);
		v = block_0(v);
		out[i] = (long) ff(v);
	}
	return -write(1, (const char *)out, sizeof(out));
}
