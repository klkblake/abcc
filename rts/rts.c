#include "syscalls.h"
#include "rts.h"

#define PARANOIA 0

char out_of_mem[] = "Out of memory\n";
char divide_by_zero[] = "Division by zero\n";
char assertion_failure[] = "Assertion failure\n";
char assertion_eq_failure[] = "Equality assertion failure\n";

const Any _Unit = (Any) (long) 0xdeadf00ddeadf00d;
const Any _deadbeef = (Any) (long) 0xdeadbeefdeadbeef;
//const Any Unit = (Any) &_Unit;
const Any Unit = (Any) 1l;
//const Any deadbeef = (Any) &_deadbeef;
const Any deadbeef = (Any) 2l;

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

#define in(ptr, heap) ((ptr) >= (heap).base && (ptr) < (heap).current)
#define heap_size(heap) ((heap).limit - (heap).base)

struct heap heap;

Any _nursery[CHUNK];
struct heap nursery = {_nursery, _nursery, _nursery + sizeof(_nursery)/sizeof(Any)};

Any stack[4096];
Any *sp = stack;

struct heap heap_new(long size) {
	struct heap heap;
	heap.base = mmap(size*sizeof(Any));
	heap.current = heap.base;
	heap.limit = heap.base + size;
	return heap;
}

void heap_free(struct heap heap) {
	munmap(heap.base, heap_size(heap)*sizeof(Any));
}

Any *heap_put(struct heap *heap, Any a, Any b) {
	if (heap->current == heap->limit) {
		DIE("Illegal heap_put()");
	}
	Any *result = heap->current;
	*heap->current++ = a;
	*heap->current++ = b;
	return result;
}

void init_mm(void) {
	heap = heap_new(CHUNK);
}

void push(Any v) {
	*sp++ = v;
}

Any pop(void) {
	return *--sp;
}

struct heap perform_major_gc(struct heap, struct heap);

void copy_value_major(Any value, struct heap nursery, struct heap old, struct heap *heap, Any *result) {
	if (in(value.as_indirect, nursery) || in(value.as_indirect, old)) {
		long tag = GET_TAG(value);
		value = CLEAR_TAG(value);
		Any target = *value.as_indirect;
		if ((target.as_tagged & (0xfff0000000000000 | FORWARDING_PTR)) == FORWARDING_PTR) {
			if (!in(target.as_indirect, *heap)) {
				copy_value_major(target, nursery, old, heap, &target);
			}
			*result = TAG(target, tag &~ FORWARDING_PTR);
		} else {
			if (heap->current == heap->limit) {
				DIE("Ran out of space while performing major GC");
			}
			Any *cell = heap->current;
			heap->current += 2;
			cell[0] = value.as_pair->fst;
			cell[1] = value.as_pair->snd;
			target.as_indirect = cell;
			*((Any *) value.as_indirect) = TAG(target, FORWARDING_PTR);
			*result = TAG(target, tag);
			copy_value_major(cell[0], nursery, old, heap, (Any *) &cell[0]);
			copy_value_major(cell[1], nursery, old, heap, (Any *) &cell[1]);
		}
	} else {
		*result = value;
	}
}

const Any _trigger_major_gc = (Any) 0l;
const Any trigger_major_gc = (Any) &_trigger_major_gc;

Any copy_value_minor(Any value, struct heap nursery, struct heap *heap) {
	if (in(value.as_indirect, nursery)) {
		long tag = GET_TAG(value);
		value = CLEAR_TAG(value);
		Any target = *value.as_indirect;
		if ((target.as_tagged & (0xfff0000000000000 | FORWARDING_PTR)) == FORWARDING_PTR) {
			value = TAG(target, tag &~ FORWARDING_PTR);
		} else {
			Any fst = copy_value_minor(value.as_pair->fst, nursery, heap);
			if (eq(fst, trigger_major_gc)) {
				return fst;
			}
			Any snd = copy_value_minor(value.as_pair->snd, nursery, heap);
			if (eq(snd, trigger_major_gc) || heap->current == heap->limit) {
				return trigger_major_gc;
			}
			target.as_indirect = heap_put(heap, fst, snd);
			*((Any *) value.as_indirect) = TAG(target, FORWARDING_PTR);
			value = TAG(target, tag);
		}
	}
	return value;
}

struct heap perform_major_gc(struct heap nursery, struct heap heap) {
	const struct heap old = heap;
	heap = heap_new(heap_size(old) + CHUNK);
	for (Any *s = stack; s < sp; s++) {
		copy_value_major(*s, nursery, old, &heap, s);
	}
	heap_free(old);
	long extra = (heap.limit - heap.current) / CHUNK - 1;
	if (extra > 0) {
		heap.limit -= extra * CHUNK;
		munmap(heap.limit, extra * CHUNK * sizeof(Any));
	}
	return heap;
}

struct heap perform_minor_gc(struct heap *nursery, struct heap heap) {
	for (Any *s = stack; s < sp; s++) {
		Any res = copy_value_minor(*s, *nursery, &heap);
		if (eq(res, trigger_major_gc)) {
			heap = perform_major_gc(*nursery, heap);
			break;
		}
		*s = res;
	}
	nursery->current = nursery->base;
	return heap;
}

long hashcode(Any v) {
	if (in(v.as_indirect, nursery) || in(v.as_indirect, heap)) {
		struct pair p = *CLEAR_TAG(v).as_pair;
		long hash = GET_TAG(v);
		hash = (hash << 13) | (hash >> (64 - 13));
		hash ^= hashcode(p.fst);
		hash = (hash << 13) | (hash >> (64 - 13));
		hash ^= hashcode(p.snd);
		return hash;
	} else {
		return v.as_tagged;
	}
}


Any *malloc(Any a, Any b) {
	// The cell size better evenly divide the chunk size, or this won't
	// fire when it needs to.
	if (nursery.current == nursery.limit) {
#if PARANOIA >= 2
		long ha1 = hashcode(a);
		long hb1 = hashcode(b);
#endif
		push(a);
		push(b);
		heap = perform_minor_gc(&nursery, heap);
		b = pop();
		a = pop();
#if PARANOIA >= 2
		long ha2 = hashcode(a);
		long hb2 = hashcode(b);
		if (ha1 != ha2 || hb1 != hb2) {
			DIE("Heap corruption");
		}
#endif
	}
	return heap_put(&nursery, a, b);
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
	if (in(a.as_indirect, nursery) || in(a.as_indirect, heap)) {
		if (in(b.as_indirect, nursery) || in(b.as_indirect, heap)) {
			long atag = GET_TAG(a);
			long btag = GET_TAG(b);
			struct pair ap = *CLEAR_TAG(a).as_pair;
			struct pair bp = *CLEAR_TAG(b).as_pair;
			return atag == btag && equiv(ap.fst, bp.fst) && equiv(ap.snd, bp.snd);
		} else {
			return 0;
		}
	} else {
		if (in(b.as_indirect, nursery) || in(b.as_indirect, heap)) {
			return 0;
		} else {
			return eq(a, b);
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
	Any a = pair(name, Unit);
	Any b = pair(a, Unit);
	Any c = pair(power, b);
	Any d = pair(Unit, c);
	Any v = pair(Unit, d);
	block_0(v);
	return 0;
}
