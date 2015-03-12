#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>
#include "array.h"

#define SYMBOL_PRODUCT 0
#define SYMBOL_NUMBER  1

internal char *names[] = {
	"*",
	"N",
};

internal u32 arities[] = {
	2,
	0,
};

struct TermNode;
struct VarNode;
DEFINE_ARRAY(struct TermNode *, termnode_ptr);
DEFINE_ARRAY(struct VarNode *, varnode_ptr);

struct TermNode {
	u64 symbol;
	struct VarNode *varNode;
	struct TermNode *term;
	struct termnode_ptr_array child;
	u64 seen;
};

struct VarNode {
	char *symbol;
	struct VarNode *rep;
	struct TermNode *term;
	u64 varCount;
	u64 termCount;
	u64 seen;
};

void PrintCyclicVar(struct VarNode *v, u64 id);

void PrintCyclicTerm(struct TermNode *t, u64 id) {
	if (t->seen != id) {
		t->seen = id;
		if (t->varNode != NULL) {
			printf("node_%lu_%p [label=\"%s\"]\n", id, t, t->varNode->symbol);
			printf("node_%lu_%p -> node_%lu_%p [label=\"var\"]\n", id, t, id, t->varNode);
			PrintCyclicVar(t->varNode, id);
		} else {
			printf("node_%lu_%p [label=\"%s\"]\n", id, t, names[t->symbol]);
		}
		if (t->term != NULL) {
			printf("node_%lu_%p -> node_%lu_%p [label=\"term\"]\n", id, t, id, t->term);
			PrintCyclicTerm(t->term, id);
		}
		foreach (c, t->child) {
			printf("node_%lu_%p -> node_%lu_%p [label=\"#%lu\"]\n", id, t, id, *c, c_index);
			PrintCyclicTerm(*c, id);
		}
	}
}

void PrintCyclicVar(struct VarNode *v, u64 id) {
	if (v->seen != id) {
		v->seen = id;
		printf("node_%lu_%p [label=\"%s (%lu, %lu)\"]\n", id, v, v->symbol, v->varCount, v->termCount);
		if (v->rep != NULL) {
			printf("node_%lu_%p -> node_%lu_%p [label=\"rep\"]\n", id, v, id, v->rep);
			PrintCyclicVar(v->rep, id);
		}
		if (v->term != NULL) {
			printf("node_%lu_%p -> node_%lu_%p [label=\"term\"]\n", id, v, id, v->term);
			PrintCyclicTerm(v->term, id);
		}
	}
}

void PrintCyclicRoot(struct TermNode *t, u64 id) {
	printf("subgraph cluster_%lu {\n", id);
	PrintCyclicTerm(t, id);
	printf("}\n");
}

struct UnificationError {
	u64 left, right;
};

internal struct varnode_ptr_array queue;

void add(struct VarNode *v, struct TermNode *t) {
	if (v->termCount == 1) {
		array_push(&queue, v);
	}
	struct TermNode *t0 = v->term;
	if (t0 == NULL) {
		v->term = t;
		t->term = t;
	} else {
		t->term = t0->term;
		t0->term = t;
	}
	v->termCount++;
}

void merge(struct VarNode *v1, struct VarNode *v2) {
	u64 r1 = v1->varCount;
	u64 r2 = v2->varCount;
	struct VarNode *bigV, *v;
	if (r1 >= r2) {
		bigV = v1;
		v = v2;
	} else {
		bigV = v2;
		v = v1;
	}
	u64 k1 = bigV->termCount;
	u64 k2 = v->termCount;
	if (k1 <= 1 && k1 + k2 > 1) {
		array_push(&queue, bigV);
	}
	struct TermNode *t0 = v->term;
	struct TermNode *t1 = bigV->term;
	if (t1 == NULL) {
		bigV->term = t0;
	} else if (t0 != NULL) {
		struct TermNode *tmp = t0->term;
		t0->term = t1->term;
		t1->term = tmp;
	}
	v->rep = bigV;
	v->term = NULL;
	v->varCount = 0;
	v->termCount = 0;
	bigV->varCount = r1 + r2;
	bigV->termCount = k1 + k2;
}

struct VarNode *rep(struct VarNode *v) {
	struct VarNode *v0 = v->rep;
	while (v0 != v0->rep) {
		v0 = v0->rep;
	}
	while (v->rep != v0) {
		struct VarNode *tmp = v->rep;
		v->rep = v0;
		v = tmp;
	}
	return v0;
}

DEFINE_ARRAY(usize, usize);

struct UnificationError commonFrontier(struct termnode_ptr_array t_list) {
	// TODO benchmark with and without checks for identical nodes
	u64 sym = t_list.data[0]->symbol;
	foreach (term, t_list) {
		if ((*term)->symbol != sym) {
			return (struct UnificationError){
				.left =  sym,
				.right = (*term)->symbol,
			};
		}
	}
	u64 a = arities[sym];
	struct termnode_ptr_array t0_list;
	t0_list.cap = t0_list.size = t_list.size;
	t0_list.data = alloca(t0_list.cap * sizeof(struct TermNode *));
	// TODO eliminate these stacks
	usize *s0_backing = alloca(t_list.size * sizeof(usize));
	usize *s1_backing = alloca(t_list.size * sizeof(usize));
	for (usize i = 0; i < a; i++) {
		for (usize j = 0; j < t_list.size; j++) {
			t0_list.data[j] = t_list.data[j]->child.data[i];
		}
		struct usize_array s0 = {};
		struct usize_array s1 = {};
		s0.size = s1.size = 0;
		s0.cap = s1.cap = t_list.size;
		s0.data = s0_backing;
		s1.data = s1_backing;
		foreach (term, t0_list) {
			if ((*term)->varNode != NULL) {
				array_push(&s0, term_index);
			} else {
				array_push(&s1, term_index);
			}
		}
		if (s0.size != 0) {
			usize j = s0.data[0];
			s0.data++;
			s0.size--;
			struct TermNode tmp = *t_list.data[0];
			*t_list.data[0] = *t_list.data[j];
			*t_list.data[j] = tmp;
			struct VarNode *v = rep(t0_list.data[j]->varNode);
			foreach (k, s0) {
				struct VarNode *v2 = rep(t0_list.data[*k]->varNode);
				if (v != v2) {
					merge(v, v2);
				}
			}
			foreach (k, s1) {
				add(v, t0_list.data[*k]);
			}
		} else {
			struct UnificationError err = commonFrontier(t0_list);
			if (err.left != err.right) {
				return err;
			}
		}
	}
	return (struct UnificationError){};
}

struct UnificationError unify(struct termnode_ptr_array t_list) {
	queue = (struct varnode_ptr_array){};
	struct UnificationError err = commonFrontier(t_list);
	if (err.left != err.right) {
		free(queue.data);
		return err;
	}
	while (queue.size > 0) {
		struct VarNode *v = queue.data[queue.size-- - 1];
		u64 k = v->termCount;
		if (k >= 2) {
			struct termnode_ptr_array t;
			t.cap = t.size = k;
			t.data = alloca(t.cap * sizeof(struct TermNode *));
			struct TermNode *t0 = v->term;
			for (u64 i = 0; i < k; i++) {
				t.data[i] = t0;
				t0 = t0->term;
			}
			t.data[0]->term = t.data[0];
			v->termCount = 1;
			err = commonFrontier(t);
			if (err.left != err.right) {
				free(queue.data);
				return err;
			}
		}
	}
	free(queue.data);
	return (struct UnificationError){};
}

int main() {
	u64 f = SYMBOL_PRODUCT;
	struct VarNode *x = &(struct VarNode){ "x", NULL, NULL, 1, 0, 0 };
	x->rep = x;
	struct VarNode *z = &(struct VarNode){ "z", NULL, NULL, 1, 0, 0 };
	z->rep = z;
	struct TermNode *x1 = &(struct TermNode){ 0, x, NULL, {}, 0 };
	struct TermNode *x2 = &(struct TermNode){ 0, x, NULL, {}, 0 };
	struct TermNode *z2 = &(struct TermNode){ 0, z, NULL, {}, 0 };
	struct termnode_ptr_array x1x1 = {(struct TermNode *[]){ x1, x1 }, 2, 2};
	struct termnode_ptr_array z2z2 = {(struct TermNode *[]){ z2, z2 }, 2, 2};
	struct termnode_ptr_array z2x2 = {(struct TermNode *[]){ z2, x2 }, 2, 2};
	struct TermNode *expr1 = &(struct TermNode){ f, NULL, NULL, x1x1, 0 };
	struct TermNode *expr2 = &(struct TermNode){ f, NULL, expr1, {(struct TermNode *[]){ &(struct TermNode){ f, NULL, NULL, z2z2, 0 }, &(struct TermNode){ f, NULL, NULL, z2x2, 0 } }, 2, 2}, 0 };
	expr1->term = expr2;
	//fmt.Printf("Expr 1: %+v\n", expr1)
	//fmt.Printf("Expr 2: %+v\n", expr2)
	printf("digraph {\n");
	PrintCyclicRoot(expr1, 1);
	struct UnificationError err = unify((struct termnode_ptr_array){(struct TermNode *[]){ expr1, expr2 }, 2, 2});
	if (err.left != err.right) {
		printf("Error: %lu, %lu\n", err.left, err.right);
		return 1;
	}
	PrintCyclicRoot(expr1, 2);
	printf("}\n");
	return 0;
}
