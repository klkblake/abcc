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
//
// Any function that allocates (pair, compBlock, sum, list[1-3]) may trigger a
// GC, which may invalidate all pointers. Hence, anything being kept around in
// registers or on the stack which points to the heap must be explicitly stored
// and then loaded from the GC stack, so that they are modified correctly.

#define CHUNK 8192

Any *base = 0;
Any *current = 0;
Any *limit = 0;

Any stack[4096];
Any *sp = stack;

void init_mm(void) {
	base = mmap(CHUNK*sizeof(Any));
	current = base;
	limit = base + 2;
}

void push(Any v) {
	*sp++ = v;
}

Any pop(void) {
	return *--sp;
}

Any adjust_value(Any *start, Any *end, Any *dest, Any value) {
	(void) end;
	if ((value.as_tagged & 0xfff0000000000000) == 0 && value.as_indirect >= start && value.as_indirect < end) {
		long tag = GET_TAG(value);
		value = CLEAR_TAG(value);
		value.as_indirect += dest - start;
		value = TAG(value, tag);
	}
	return value;
}

Any *heap_copy(Any *start, Any *end, Any *dest) {
	Any *src = start;
	Any *dest_start = dest;
	while (src != end) {
		*dest++ = adjust_value(start, end, dest_start, *src++);
	}
	return dest;
}

void stack_fixup(Any *start, Any *end, Any *dest) {
	Any *p = stack;
	while (p != sp) {
		Any v = *p;
		*p++ = adjust_value(start, end, dest, v);
	}
}

Any *malloc(Any a, Any b) {
	// The cell size better evenly divide the chunk size, or this won't
	// fire when it needs to.
	if (current == limit) {
		long oldsize = limit - base;
		long newsize = oldsize + CHUNK;
		Any *new = mmap(newsize * sizeof(Any));
		current = heap_copy(base, limit, new);
		stack_fixup(base, limit, new);
		a = adjust_value(base, limit, new, a);
		b = adjust_value(base, limit, new, b);
		for (int i = 0; i < oldsize; i++) {
			base[i] = (Any) 0l;
		}
		munmap(base, oldsize * sizeof(Any));
		base = new;
		limit = new + newsize;
	}
	Any *result = current;
	*current++ = a;
	*current++ = b;
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
