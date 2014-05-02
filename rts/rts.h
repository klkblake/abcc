struct pair;
struct comp_block;

enum block_type {
	BLOCK_NORMAL,
	BLOCK_COMP,
	BLOCK_QUOTE,
};

// Must match Codegen.hs
enum sum_side {
	SUM_LEFT,
	SUM_RIGHT,
};

union any {
	const struct pair *as_pair;
	union any (*as_block)(union any);
	const struct comp_block *as_comp_block;
	const union any *as_indirect;
	double as_num;
	long as_tagged;
};

typedef union any Any;

// Doubles are stored inverted, so that pointers correspond to NaNs and Infs,
// which can't occur in ABC.
#define TO_N(n) ((Any) ~((Any) (n)).as_tagged)
#define N(v) (((Any) ~(v).as_tagged).as_num)

struct pair {
	const Any fst, snd;
} __attribute__((packed));

typedef const struct pair *Pair;

typedef Any (*Block)(Any);

struct comp_block {
	const Any xy, yz;
} __attribute__((packed));

typedef const struct comp_block *CompBlock;

extern const Any Unit;

#define CLEAR_TAG(v) ((Any) ((v).as_tagged &~ 0x3))
#define TAG(v, t) ((Any) (CLEAR_TAG(v).as_tagged | t))
#define GET_TAG(v) ((v).as_tagged & 0x3)

#define f(v) (((v).as_pair)->fst)
#define s(v) (((v).as_pair)->snd)

#define v0 f(v)
#define v1 f(s(v))
#define v2 f(s(s(v)))

#define vt1 s(v)
#define vt2 s(s(v))
#define vt3 s(s(s(v)))

#define list1(a, l) pair(a, l)
#define list2(a, b, l) pair(a, pair(b, l))
#define list3(a, b, c, l) pair(a, pair(b, pair(c, l)))

#define deref(v) (*(CLEAR_TAG(v).as_indirect))

#define s0 f(v)
#define s1 deref(s0)
#define s2 deref(s1)

#define EITHER(v, l, r) (GET_TAG(v) == SUM_LEFT) ? (l) : (r)
#define EITHER3(v1, v2, l, m, r) EITHER(v1, l, EITHER(v2, m, r))
#define EITHER4(v1, v2, v3, b0, b1, b2, b3) EITHER3(v1, v2, b0, b1, EITHER(v3, b2, b3))

Any pair(Any a, Any b);

#define OPFUNC(name) Any name(Any v)

#define OP(name, expr) OPFUNC(name) { return (expr); }
#define OP1(name, expr) OP(name, list1((expr), vt1))
#define OP21(name, expr) OP(name, list1((expr), vt2))

#define op(name) OPFUNC(name)
op(assocl);
op(assocr);
op(swap);
op(swapd);
op(intro1);
op(elim1);
op(copy);
op(drop);
Any applyBlock(Any b, Any x);
op(apply);
op(apply_tail);
op(compose);
op(quote);
op(introNum);
Any digit(int d, Any v);
op(add);
op(multiply);
op(inverse);
op(negate);
op(divmod);
op(assocls);
op(assocrs);
op(swaps);
op(swapds);
op(intro0);
op(elim0);
op(condapply);
op(distrib);
op(factor);
op(merge);
Any _assert(char *line, int size, Any v);
#define _STRINGIFY(line) #line
#define STRINGIFY(line) _STRINGIFY(line)
#define ASSERT(line, v) _assert(line, sizeof(line) - 1, v)
#define assert(v) ASSERT("Line " STRINGIFY(__LINE__) ": ", (v))
op(greater);
op(debug_print_raw);
op(debug_print_text);
#undef op
