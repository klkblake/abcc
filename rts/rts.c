#include "syscalls.h"
#include "rts.h"

char out_of_mem[] = "Out of memory\n";
char divide_by_zero[] = "Division by zero\n";
char assertion_failure[] = "Assertion failure\n";
char assertion_eq_failure[] = "Equality assertion failure\n";

const Any _Unit = (Any) (long) 0xdeadf00ddeadf00d;
const Any _deadbeef = (Any) (long) 0xdeadbeefdeadbeef;
const Any Unit = (Any) &_Unit;
const Any deadbeef = (Any) &_deadbeef;

// The memory manager uses a cons cell memory model. All allocations are
// exactly 2 words in length, where a word is assumed to be 64 bits.
// Additionally, pointers and numbers are distinguished by examining the top 12
// bits -- on current amd64 architectures, all pointers have these bits all
// equal. The only valid float permitted by ABC's numeric model which has the
// top 12 bits all equal is zero. Hence, care must be taken to not use null
// pointers in the implementation.
//
// Any function that allocates (pair, compBlock, sum, list[1-3]) may trigger a
// GC, which may invalidate all pointers. Hence, anything being kept around in
// registers or on the stack which points to the heap must be explicitly stored
// and then loaded from the GC stack, so that they are modified correctly.

#define CHUNK 8192

struct heap {
	Any *base;
	Any *current;
	Any *limit;
};

struct heap heap;

Any stack[4096];
Any *sp = stack;

void init_mm(void) {
	heap.base = mmap(CHUNK*sizeof(Any));
	heap.current = heap.base;
	heap.limit = heap.base + CHUNK;
}

void push(Any v) {
	*sp++ = v;
}

Any pop(void) {
	return *--sp;
}

Any copy_value(Any value, Any *start, Any *end) {
	if (value.as_indirect >= start && value.as_indirect < end) {
		long tag = GET_TAG(value);
		value = CLEAR_TAG(value);
		Any target = *value.as_indirect;
		if ((target.as_tagged & (0xfff0000000000000 | FORWARDING_PTR)) == FORWARDING_PTR) {
			value = TAG(target, tag &~ FORWARDING_PTR);
		} else {
			Any fst = copy_value(value.as_pair->fst, start, end);
			Any snd = copy_value(value.as_pair->snd, start, end);
			target.as_indirect = heap.current;
			*heap.current++ = fst;
			*heap.current++ = snd;
			*((Any *) value.as_indirect) = TAG(target, FORWARDING_PTR);
			value = TAG(target, tag);
		}
	}
	return value;
}

Any *malloc(Any a, Any b) {
	// The cell size better evenly divide the chunk size, or this won't
	// fire when it needs to.
	if (heap.current == heap.limit) {
		long oldsize = heap.limit - heap.base;
		long newsize = oldsize + CHUNK;
		Any *old = heap.base;
		Any *oldlimit = heap.limit;
		heap.base = mmap(newsize * sizeof(Any));
		heap.current = heap.base;
		heap.limit = heap.base + newsize;
		for (Any *s = stack; s < sp; s++) {
			*s = copy_value(*s, old, oldlimit);
		}
		a = copy_value(a, old, oldlimit);
		b = copy_value(b, old, oldlimit);
		munmap(old, oldsize * sizeof(Any));
		long extra = (heap.limit - heap.current) / CHUNK - 1;
		if (extra > 0) {
			heap.limit -= extra * CHUNK;
			munmap(heap.limit, extra * CHUNK * sizeof(Any));
		}
	}
	Any *result = heap.current;
	*heap.current++ = a;
	*heap.current++ = b;
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

#define list1(a, l) pair(a, l)

Any list2(Any a, Any b, Any l) {
	push(a);
	Any p = pair(b, l);
	return pair(pop(), p);
}

Any list3(Any a, Any b, Any c, Any l) {
	push(a);
	Any p = list2(b, c, l);
	return pair(pop(), p);
}

#define OP(name, expr) OPFUNC(name) { return (expr); }
#define OP1(name, expr) OPFUNC(name) { push(vt1); Any val = (expr); return list1(val, pop()); }
#define OP21(name, expr) OPFUNC(name) { push(vt2); Any val = (expr); return list1(val, pop()); }

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
		push(cb->yz);
		x = applyBlock(cb->xy, x);
		x = applyBlock(pop(), x);
		break;

		case BLOCK_QUOTE:
		x = pair(*(b.as_indirect), x);
		break;
	}
	return x;
}

// ---- BEGIN OPS ----

// -- Basic plumbing --

OPFUNC(assocl) {
	push(s(s(v)));
	Any p = pair(f(v), f(s(v)));
	return pair(p, pop());
}

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

OP1(quote, sum(v0, BLOCK_QUOTE));

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

OPFUNC(distrib) {
	long tag = GET_TAG(v1);
	push(vt2);
	Any val = sum(pair(v0, deref(v1)), tag);
	return list1(val, pop());
}

OPFUNC(factor) {
	long tag = GET_TAG(s0);
	push(vt1);
	push(s(s1));
	Any a = sum(f(s1), tag);
	Any top = pop();
	push(a);
	Any b = sum(top, tag);
	a = pop();
	return list2(a, b, pop());
}

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
	push(vt2);
	Any a = sum(pair(TO_N(g ? x : y), TO_N(g ? y : x)), g ? SUM_RIGHT : SUM_LEFT);
	return list1(a, pop());
}

// XXX This is incomplete -- it does not handle blocks correctly.
long equiv(Any a, Any b) {
	if (a.as_indirect >= heap.base && a.as_indirect < heap.limit) {
		if (b.as_indirect >= heap.base && b.as_indirect < heap.limit) {
			long atag = GET_TAG(a);
			long btag = GET_TAG(b);
			struct pair ap = *CLEAR_TAG(a).as_pair;
			struct pair bp = *CLEAR_TAG(b).as_pair;
			return atag == btag && equiv(ap.fst, bp.fst) && equiv(ap.snd, bp.snd);
		} else {
			return 0;
		}
	} else {
		if (b.as_indirect >= heap.base && b.as_indirect < heap.limit) {
			return 0;
		} else {
			return a.as_tagged == b.as_tagged;
		}
	}
}

Any _assert_eq(char *line, int size, Any v) {
	Any a = v0;
	Any b = v1;
	if (!equiv(a, b)) {
		write(2, line, size);
		DIE(assertion_eq_failure);
	}
	return v;
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
	block_0(v);
	return 0;
}
