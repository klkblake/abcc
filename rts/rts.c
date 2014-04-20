#include "syscalls.h"
#include "rts.h"

const Any Unit = (Any) 1l;

#define DIE(name) do { write(2, name, sizeof(name)); exit(1); } while (0)
char out_of_mem[] = "Out of memory\n";
char divide_by_zero[] = "Division by zero\n";
char assertion_failure[] = ": Assertion failure\n";
char write_failed[] = "Write failed\n";

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

Any sum(Any s, long tag) {
	Any *result = malloc(sizeof(Any));
	*result = s;
	return TAG((Any) (const Any *) result, tag);
}

OP(assocl) {
	return pair(pair(f(v), f(s(v))), s(s(v)));
}

OP(assocr) {
	return list2(f(f(v)), s(f(v)), s(v));
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
		x = pair(*(b.as_indirect), x);
		break;
	}
	return x;
}

OP21(apply, applyBlock(v0, v1));

OP(apply_tail) {
	return applyBlock(v0, v1);
}

OP21(compose, compBlock(v1, v0));

OP1(quote, TAG((Any) &v0, BLOCK_QUOTE));

OP(introNum) {
	return list1((Any) 0.0, v);
}

Any digit(int d, Any v) {
	return list1((Any) (v0.as_num * 10 + d), vt1);
}

OP21(add, (Any) (v0.as_num + v1.as_num));

OP21(multiply, (Any) (v0.as_num * v1.as_num));

OP(inverse) {
	if (v0.as_num == 0) {
		DIE(divide_by_zero);
	}
	return list1((Any) (1 / v0.as_num), vt1);
}

OP1(negate, (Any) (-v0.as_num));

typedef double v2df __attribute__((vector_size(16)));

OP(divmod) {
	double a = v1.as_num, b = v0.as_num;
	v2df q = {a / b};
	q = __builtin_ia32_roundsd(q, q, 0x1);
	return list2((Any) (a - q[0]*b), (Any) q[0], vt2);
}

OP1(assocls, EITHER3(s0, s1,
			sum(s0, SUM_LEFT),
			sum(TAG(s1, SUM_RIGHT), SUM_LEFT),
			s1));

OP1(assocrs, EITHER(s0,
			EITHER(s1,
				s1,
				sum(TAG(s1, SUM_LEFT), SUM_RIGHT)),
			sum(s0, SUM_RIGHT)));

OP1(swaps, EITHER3(s0, s1,
			sum(s0, SUM_RIGHT),
			s1,
			s0));

OP1(swapds, EITHER4(s0, s1, s2,
			s0,
			sum(s0, SUM_RIGHT),
			s1,
			s0));

OP1(intro0, sum(v0, SUM_LEFT));

OP1(elim0, s1);

OP21(condapply, EITHER(v1,
			sum(applyBlock(v0, deref(v1)), SUM_LEFT),
			v1));

OP21(distrib, sum(pair(v0, deref(v1)), GET_TAG(v1)));

OP(factor) {
	return list2(sum(f(s1), GET_TAG(s0)), sum(s(s1), GET_TAG(s0)) , vt1);
}

OP1(merge, s1);

Any _assert(char *line, int size, Any v) {
	if (GET_TAG(s0) == SUM_LEFT) {
		write(2, line, size);
		DIE(assertion_failure);
	}
	return list1(s1, vt1);
}

OP(greater) {
	double x = v0.as_num;
	double y = v1.as_num;
	int g = y > x;
	return list1(sum(pair((Any) (g ? x : y), (Any) (g ? y : x)), g ? SUM_RIGHT : SUM_LEFT), vt2);
}

void write_force(int fd, const char *buf, long size) {
	long res = write(fd, buf, size);
	if (res != 0) {
		DIE(write_failed);
	}
}

OP(debug_print_raw) {
	write_force(2, (const char *) &v0, sizeof(v0));
	return vt1;
}

void debug_print_text2(Any v) {
	char buf[256];
	int size = 0;
	while (GET_TAG(v) == SUM_RIGHT) {
		v = *CLEAR_TAG(v).as_indirect;
		buf[size++] = (char) (long) f(v).as_num;
		if (size == sizeof(buf)) {
			write_force(2, buf, size);
			size = 0;
		}
		v = s(v);
	}
	if (size > 0) {
		write_force(2, buf, size);
	}
}

OP(debug_print_text) {
	debug_print_text2(v0);
	return vt1;
}

extern Any block_0(Any);

int main(void) {
	init_mm();
	Any power = Unit;
	Any name = sum(Unit, SUM_LEFT);
	Any v = pair(Unit, pair(Unit, pair(power, pair(pair(name, Unit), Unit))));
	v = block_0(v);
	return 0;
}
