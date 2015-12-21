#include "build_graphs.c"

typedef struct {
	u32 first;
	u32 size;
} Segment;

typedef struct {
	u32 first;
	u32 size;
	u32 counter;
	U32Array move;
} ClassHead;

typedef enum {
	TRANSITION_UNIT,
	TRANSITION_NUMBER,
	TRANSITION_PRODUCT_1,
	TRANSITION_PRODUCT_2,
	TRANSITION_SUM_1,
	TRANSITION_SUM_2,
	TRANSITION_BLOCK_1,
	TRANSITION_BLOCK_2,
	TRANSITION_POLYMORPHIC_BLOCK_1,
	TRANSITION_POLYMORPHIC_BLOCK_2,

	NUM_TRANSITIONS,
} Transition;

typedef struct {
	u32 cls_of;
	u32 idx_of;
} State;

internal inline
U32Array collect(Transition x, u32 C) {
	Segment *inv_head;   // X x A
	u32 *inv_elts;       // X x A
	ClassHead *cls_head; // A
	u32 *cls_elts;       // A
	State *states;
	//
	// This is reversed from the paper -- we push onto the end
	U32Array suspects = {};
	ClassHead CH = cls_head[C];
	for (u32 i = CH.first; i < CH.first + CH.size; i++) {
		Segment inv_x_a = inv_head[x + cls_elts[i] * NUM_TRANSITIONS];
		for (u32 j = inv_x_a.first; j < inv_x_a.first + inv_x_a.size; j++) {
			u32 b = inv_elts[j];
			u32 B = states[b].cls_of;
			if (cls_head[B].counter == 0) {
				array_push(&suspects, B);
				array_free(&cls_head[B].move);
				cls_head[B].move = (U32Array){}; // WTF?
			}
			cls_head[B].counter++;
			array_push(&cls_head[B].move, b);
		}
	}
	return suspects;
}

internal inline
Type *minimise_dfa(Segment *inv_head, u32 *inv_elts, Segment *cls_head, u32 *cls_elts, State *states) {
}

internal
Type *canonical_type(Type *type, TypePool *pool) {
	type = deref(type);
	if (IS_VAR(type)) {
		return type;
	}
	if (type->symbol == SYMBOL_UNIT) {
		return pool->unit;
	}
	if (type->symbol == SYMBOL_NUMBER) {
		return pool->number;
	}
	if (type->symbol == SYMBOL_BOOL) {
		return pool->boolean;
	}
}
