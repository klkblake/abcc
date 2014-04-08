#define Any const void *

struct pair {
	const void *fst, *snd;
};

typedef const struct pair *Pair;

enum block_type {
	BLOCK_NORMAL,
	BLOCK_COMP,
	BLOCK_QUOTE,
};

typedef Any (*Block)(Any);

struct comp_block {
	Block xy, yz;
};

typedef const struct comp_block *CompBlock;

#define Unit ((Any) 0x1)
#define Void ((Any) 0x0)

Pair pair(Any a, Any b);

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
OP(add);
OP(multiply);
