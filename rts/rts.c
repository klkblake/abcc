#include "syscalls.h"
#include "rts.h"

#define DIE(name) do { write(2, name, sizeof(name)); exit(1); } while (0)
char out_of_mem[] = "Out of memory\n";
char divide_by_zero[] = "Division by zero\n";

void *base = 0;
void *limit = 0;

void init_mm(void) {
	base = brk(0);
	limit = base;
}

void *malloc(long size) {
	if (limit - base < size) {
		void *old = limit;
		limit = brk(limit + 16*4096);
		if (limit == old) {
			DIE(out_of_mem);
		}
	}
	void *result = base;
	base += size;
	return result;
}

Any pair(Any a, Any b) {
	union any *result = malloc(sizeof(struct pair));
	result[0] = a;
	result[1] = b;
	return (Any) (Pair) result;
}

Any compBlock(Any a, Any b) {
	union any *result = malloc(sizeof(struct comp_block));
	result[0] = a;
	result[1] = b;
	return TAG((Any) (CompBlock) result, BLOCK_COMP);
}

#define f(v) (((v).as_pair)->fst)
#define s(v) (((v).as_pair)->snd)

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
	return vt1;
}

OP(copy) {
	return list2(v0, v0, vt1);
}

Any applyBlock(Any b, Any x) {
	long type = GET_TAG(b);
	b = CLEAR_TAG(b);
	CompBlock cb;
	switch (type) {
		case BLOCK_NORMAL:
		x = b.as_block(x);
		break;

		case BLOCK_COMP:
		cb = b.as_comp_block;
		x = applyBlock(cb->xy, x);
		x = applyBlock(cb->yz, x);
		break;

		case BLOCK_QUOTE:
		x = pair(*(b.as_quote_block), x);
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

OP(quote) {
	return list1(TAG((Any) &v0, BLOCK_QUOTE), vt1);
}

OP(introNum) {
	return list1((Any) 0.0, v);
}

Any digit(int d, Any v) {
	return list1((Any) (v0.as_num * 10 + d), vt1);
}

OP(add) {
	return list1((Any) (v0.as_num + v1.as_num), vt2);
}

OP(multiply) {
	return list1((Any) (v0.as_num * v1.as_num), vt2);
}

OP(inverse) {
	if (v0.as_num == 0) {
		DIE(divide_by_zero);
	}
	return list1((Any) (1 / v0.as_num), vt1);
}

OP(negate) {
	return list1((Any) (-v0.as_num), vt1);
}

typedef double v2df __attribute__((vector_size(16)));

OP(divmod) {
	double a = v1.as_num, b = v0.as_num;
	v2df q = {a / b};
	q = __builtin_ia32_roundsd(q, q, 0x1);
	return list2((Any) q[0], (Any) (a - q[0]*b), vt2);
}

#define COUNT 1000

extern Any block_0(Any);

int main(void) {
	init_mm();
	double out[COUNT];
	for (long i = 0; i < COUNT; i++) {
		Any v = pair(pair((Any) (double) i, Unit), Unit);
		v = block_0(v);
		out[i] = ff(v).as_num;
	}
	return -write(1, (const char *)out, sizeof(out));
}
