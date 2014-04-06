#include "syscalls.h"
#include "ops.h"

struct pair {
	const void *fst, *snd;
};

#define Pair const struct pair *
#define to_pair(v) ((Pair) (v))

#define Unit ((Any) 0x1)
#define Void ((Any) 0x0)

#define DIE(name) do { write(2, name, sizeof(name)); exit(1); } while (0)
char out_of_mem[] = "Out of memory\n";

void *base = 0;
void *limit = 0;

void init_mm(void) {
	base = brk(0);
	limit = base;
}

void *malloc(long size) {
	/*
	if (base == &end) {
		write(2, &base, 8);
		base = brk(0);
		write(2, &base, 8);
		write(2, &limit, 8);
		exit(0);
	}
	*/
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

#define f(v) (to_pair(v)->fst)
#define s(v) (to_pair(v)->snd)

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

OP(add) {
	return list1((Any) ((long)v0 + (long) v1), vt2);
}

OP(multiply) {
	return list1((Any) ((long)v0 * (long) v1), vt2);
}

#define COUNT 1000

extern Any run_abc(Any);

int main(void) {
	init_mm();
	long out[COUNT];
	for (long i = 0; i < COUNT; i++) {
		Any v = pair(pair((void *) i, Unit), Unit);
		v = run_abc(v);
		out[i] = (long) ff(v);
	}
	return -write(1, (const char *)out, sizeof(out));
}
