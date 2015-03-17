#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>

#include "array.h"
#include "map.h"
#include "string.h"
#include "type.h"

internal char *names[] = {
	"0",
	"1",
	"N",
	"*",
	"+",
	"[->]",
};

internal u32 arities[] = {
	0,
	0,
	0,
	2,
	2,
	2,
};

extern u32 type_hash(union type *key);

DEFINE_MAP(union type *, b1, type_ptr_b1, type_hash);

void print_type(union type *t, u64 id, struct type_ptr_b1_map *seen) {
	struct type_ptr_b1_map_get_result result = type_ptr_b1_map_get(seen, t);
	if (!result.found) {
		type_ptr_b1_map_put_bucket(seen, t, true, result.bucket);
		if (IS_VAR(t)) {
			printf("node_%lu_%p [label=\"%lu, %llu\"]\n", id, t, t->var_count, t->term_count &~ VAR_BIT);
			if (t->rep != NULL) {
				printf("node_%lu_%p -> node_%lu_%p [label=\"rep\"]\n", id, t, id, t->rep);
				print_type(t->rep, id, seen);
			}
			if (t->terms != NULL) {
				printf("node_%lu_%p -> node_%lu_%p [label=\"terms\"]\n", id, t, id, t->terms);
				print_type(t->terms, id, seen);
			}
		} else {
			if (t->next != NULL) {
				printf("node_%lu_%p -> node_%lu_%p [label=\"next\"]\n", id, t, id, t->next);
				print_type(t->next, id, seen);
			}
			u32 arity;
			if (IS_SEALED(t->symbol)) {
				printf("node_%lu_%p [label=\"Sealed: \\\"", id, t);
				fwrite(t->seal->data, 1, t->seal->size, stdout);
				printf("\\\"\"]\n");
				arity = 1;
			} else {
				printf("node_%lu_%p [label=\"%s\"]\n", id, t,
				       names[t->symbol &~ (HIGH_PTR_BIT | POLYMORPHIC_BIT)]);
				arity = arities[t->symbol &~ (HIGH_PTR_BIT | POLYMORPHIC_BIT)];
			}
			if (arity > 0){
				printf("node_%lu_%p -> node_%lu_%p [label=\"#0\"]\n", id, t, id, t->child1);
				print_type(t->child1, id, seen);
				if (arity > 1) {
					printf("node_%lu_%p -> node_%lu_%p [label=\"#1\"]\n", id, t, id, t->child2);
					print_type(t->child2, id, seen);
				}
			}
		}
	}
}

void print_type_root(union type *t, u64 id) {
	struct type_ptr_b1_map seen = {};
	printf("subgraph cluster_%lu {\n", id);
	print_type(t, id, &seen);
	printf("}\n");
	map_free(&seen);
}

struct UnificationError {
	u64 left, right;
};

internal struct type_ptr_array queue;

void add(union type *v, union type *t) {
	if ((v->term_count &~ VAR_BIT) == 1) {
		array_push(&queue, v);
	}
	union type *t0 = v->terms;
	if (t0 == NULL) {
		v->terms = t;
		t->next = t;
	} else {
		t->next = t0->next;
		t0->next = t;
	}
	v->term_count++;
}

void merge(union type *v1, union type *v2) {
	u64 r1 = v1->var_count;
	u64 r2 = v2->var_count;
	union type *bigV, *v;
	if (r1 >= r2) {
		bigV = v1;
		v = v2;
	} else {
		bigV = v2;
		v = v1;
	}
	u64 k1 = bigV->term_count &~ VAR_BIT;
	u64 k2 = v->term_count &~ VAR_BIT;
	if (k1 <= 1 && k1 + k2 > 1) {
		array_push(&queue, bigV);
	}
	union type *t0 = v->terms;
	union type *t1 = bigV->terms;
	if (t1 == NULL) {
		bigV->terms = t0;
	} else if (t0 != NULL) {
		union type *tmp = t0->next;
		t0->next = t1->next;
		t1->next = tmp;
	}
	v->rep = bigV;
	v->terms = NULL;
	v->var_count = 0;
	v->term_count = VAR_BIT;
	bigV->var_count = r1 + r2;
	bigV->term_count = (k1 + k2) | VAR_BIT;
}

union type *rep(union type *v) {
	union type *v0 = v->rep;
	while (v0 != v0->rep) {
		v0 = v0->rep;
	}
	while (v->rep != v0) {
		union type *tmp = v->rep;
		v->rep = v0;
		v = tmp;
	}
	return v0;
}

DEFINE_ARRAY(usize, usize);

struct UnificationError commonFrontier(struct type_ptr_array t_list) {
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
	u64 a;
	if (IS_SEALED(sym)) {
		a = 1;
	} else {
		a = arities[sym &~ (HIGH_PTR_BIT | POLYMORPHIC_BIT)];
	}
	struct type_ptr_array t0_list;
	t0_list.cap = t0_list.size = t_list.size;
	t0_list.data = alloca(t0_list.cap * sizeof(union type *));
	// TODO eliminate these stacks
	usize *s0_backing = alloca(t_list.size * sizeof(usize));
	usize *s1_backing = alloca(t_list.size * sizeof(usize));
	for (usize i = 0; i < a; i++) {
		for (usize j = 0; j < t_list.size; j++) {
			t0_list.data[j] = (&t_list.data[j]->child1)[i];
		}
		struct usize_array s0 = {};
		struct usize_array s1 = {};
		s0.size = s1.size = 0;
		s0.cap = s1.cap = t_list.size;
		s0.data = s0_backing;
		s1.data = s1_backing;
		foreach (term, t0_list) {
			if (IS_VAR(*term)) {
				array_push(&s0, term_index);
			} else {
				array_push(&s1, term_index);
			}
		}
		if (s0.size != 0) {
			usize j = s0.data[0];
			s0.data++;
			s0.size--;
			union type tmp = *t_list.data[0];
			*t_list.data[0] = *t_list.data[j];
			*t_list.data[j] = tmp;
			union type *v = rep(t0_list.data[j]);
			foreach (k, s0) {
				union type *v2 = rep(t0_list.data[*k]);
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

struct UnificationError unify(struct type_ptr_array t_list) {
	queue = (struct type_ptr_array){};
	struct UnificationError err = commonFrontier(t_list);
	if (err.left != err.right) {
		free(queue.data);
		return err;
	}
	while (queue.size > 0) {
		union type *v = array_pop(&queue);
		u64 k = v->term_count &~ VAR_BIT;
		if (k >= 2) {
			struct type_ptr_array t;
			t.cap = t.size = k;
			t.data = alloca(t.cap * sizeof(union type *));
			union type *t0 = v->terms;
			for (u64 i = 0; i < k; i++) {
				t.data[i] = t0;
				t0 = t0->next;
			}
			t.data[0]->next = t.data[0];
			v->term_count = 1 | VAR_BIT;
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

#define prod(n, c1, c2) union type n = { { { SYMBOL_PRODUCT }, NULL, &c1, &c2 } }; n.next = &n

int main() {
	union type x = {};
	x.rep = &x;
	x.term_count = VAR_BIT;
	union type z = {};
	z.rep = &z;
	z.term_count = VAR_BIT;
	prod(expr1, x, x);
	prod(c1, z, z);
	prod(c2, z, x);
	prod(expr2, c1, c2);
	expr1.next = &expr2;
	expr2.next = &expr1;
	printf("digraph {\n");
	print_type_root(&expr1, 1);
	struct UnificationError err = unify((struct type_ptr_array){(union type *[]){ &expr1, &expr2 }, 2, 2});
	if (err.left != err.right) {
		printf("Error: %lu, %lu\n", err.left, err.right);
		return 1;
	}
	print_type_root(&expr1, 2);
	printf("}\n");
	return 0;
}
