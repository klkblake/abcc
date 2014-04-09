struct pair;
struct comp_block;

enum block_type {
	BLOCK_NORMAL,
	BLOCK_COMP,
	BLOCK_QUOTE,
};

union any {
	const struct pair *as_pair;
	union any (*as_block)(union any);
	const struct comp_block *as_comp_block;
	const union any *as_quote_block;
	double as_num;
	long as_tagged;
};

typedef union any Any;

struct pair {
	const Any fst, snd;
};

typedef const struct pair *Pair;

typedef const Any (*Block)(const Any);

struct comp_block {
	const Any xy, yz;
};

typedef const struct comp_block *CompBlock;

#define Unit ((Any) (long) 0x1)
#define Void ((Any) (long) 0x0)

#define TAG(v, t) ((Any) ((v).as_tagged | t))
#define GET_TAG(v) ((v).as_tagged & 0x3)
#define CLEAR_TAG(v) ((Any) ((v).as_tagged &~ 0x3))

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
OP(apply);
OP(compose);
OP(quote);
OP(introNum);
Any digit(int d, Any v);
OP(add);
OP(multiply);
