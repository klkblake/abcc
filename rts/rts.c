#include "syscalls.h"
#include "rts.h"

char out_of_mem[] = "Out of memory\n";
char divide_by_zero[] = "Division by zero\n";
char assertion_failure[] = "Assertion failure\n";

const Any Unit = (Any) (long) 0xdeadf00ddeadf00d;
const Any deadbeef = (Any) (long) 0xdeadbeefdeadbeef;

// The memory manager uses a cons cell memory model. All allocations are
// exactly 2 words in length, where a word is assumed to be 64 bits.
// Additionally, pointers and numbers are distinguished by examining the top 12
// bits -- on current amd64 architectures, all pointers have these bits all
// equal. The only valid float permitted by ABC's numeric model which has the
// top 12 bits all equal is zero. Hence, care must be taken to not use null
// pointers in the implementation.

Any *base = 0;
Any *limit = 0;

void init_mm(void) {
	base = brk(0);
	limit = base;
}

Any *malloc(Any a, Any b) {
	// The cell size better evenly divide the page size, or this won't fire
	// when it needs to.
	if (base == limit) {
		void *old = limit;
		limit = brk(limit + 16*4096);
		if (limit == old) {
			DIE(out_of_mem);
		}
	}
	Any *result = base;
	*base++ = a;
	*base++ = b;
	return result;
}

Any pair(Any a, Any b) {
	return (Any) (Pair) malloc(a, b);
}

Any compBlock(Any a, Any b) {
	return TAG((Any) (CompBlock) malloc(a, b), BLOCK_COMP);
}

Any sum(Any s, long tag) {
	return TAG((Any) (const Any *) malloc(s, deadbeef), tag);
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

// ---- BEGIN OPS ----

// -- Basic plumbing --

OP(assocl, pair(pair(f(v), f(s(v))), s(s(v))));

OP(assocr, list2(f(f(v)), s(f(v)), s(v)));

OP(swap, list2(v1, v0, vt2));

OP(swapd, list3(v0, v2, v1, vt3));

OP(intro1, pair(v, Unit));

OP(elim1, v0);

OP(drop, vt1);

OP(copy, list2(v0, v0, vt1));

// -- Block operations --

OP21(apply, applyBlock(v0, v1));

OP(apply_tail, applyBlock(v0, v1));

OP21(compose, compBlock(v1, v0));

OP1(quote, TAG((Any) &v0, BLOCK_QUOTE));

// -- Math --

OP(introNum, list1(TO_N(0.0), v));

Any digit(int d, Any v) {
	return list1(TO_N(N(v0) * 10 + d), vt1);
}

OP21(add, TO_N(N(v0) + N(v1)));

OP21(multiply, TO_N(N(v0) * N(v1)));

OPFUNC(inverse) {
	if (N(v0) == 0) {
		DIE(divide_by_zero);
	}
	return list1(TO_N(1 / N(v0)), vt1);
}

OP1(negate, TO_N(-N(v0)));

typedef double v2df __attribute__((vector_size(16)));

OPFUNC(divmod) {
	double a = N(v1), b = N(v0);
	v2df q = {a / b};
	q = __builtin_ia32_roundsd(q, q, 0x1);
	return list2(TO_N(a - q[0]*b), TO_N(q[0]), vt2);
}

// -- Basic sum plumbing --

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

// -- Other sum operations --

OP21(condapply, EITHER(v1,
			sum(applyBlock(v0, deref(v1)), SUM_LEFT),
			v1));

OP21(distrib, sum(pair(v0, deref(v1)), GET_TAG(v1)));

OP(factor, list2(sum(f(s1), GET_TAG(s0)), sum(s(s1), GET_TAG(s0)) , vt1));

OP1(merge, s1);

// -- Misc --

Any _assert(char *line, int size, Any v) {
	if (GET_TAG(s0) == SUM_LEFT) {
		write(2, line, size);
		DIE(assertion_failure);
	}
	return list1(s1, vt1);
}

OPFUNC(greater) {
	double x = N(v0);
	double y = N(v1);
	int g = y > x;
	return list1(sum(pair(TO_N(g ? x : y), TO_N(g ? y : x)), g ? SUM_RIGHT : SUM_LEFT), vt2);
}

OPFUNC(debug_print_raw) {
	write_force(2, (const char *) &v0, sizeof(v0));
	return vt1;
}

void debug_print_text2(Any v) {
	char buf[256];
	int size = 0;
	while (GET_TAG(v) == SUM_RIGHT) {
		v = *CLEAR_TAG(v).as_indirect;
		buf[size++] = (char) (long) N(f(v));
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

OPFUNC(debug_print_text) {
	debug_print_text2(v0);
	return vt1;
}

// ---- END OPS ----

extern Any block_0(Any);

int main(void) {
	init_mm();
	Any power = Unit;
	Any name = sum(Unit, SUM_LEFT);
	Any v = pair(Unit, pair(Unit, pair(power, pair(pair(name, Unit), Unit))));
	v = block_0(v);
	return 0;
}
