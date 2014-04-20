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

struct pair {
	const Any fst, snd;
};

typedef const struct pair *Pair;

typedef Any (*Block)(Any);

struct comp_block {
	const Any xy, yz;
};

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

#define OP(name) Any name(Any v)

OP(assocl);
OP(assocr);
OP(swap);
OP(swapd);
OP(intro1);
OP(elim1);
OP(copy);
OP(drop);
Any applyBlock(Any b, Any x);
OP(apply);
OP(apply_tail);
OP(compose);
OP(quote);
OP(introNum);
Any digit(int d, Any v);
OP(add);
OP(multiply);
OP(inverse);
OP(negate);
OP(divmod);
OP(assocls);
OP(assocrs);
OP(swaps);
OP(swapds);
OP(intro0);
OP(elim0);
OP(condapply);
OP(distrib);
OP(factor);
OP(merge);
Any _assert(char *line, int size, Any v);
#define __stringify(line) #line
#define __str(line) __stringify(line)
#define _assert2(line, v) _assert(line, sizeof(line), v)
#define assert(v) _assert2("Line " __str(__LINE__), (v))
OP(greater);
OP(debug_print_raw);
OP(debug_print_text);
