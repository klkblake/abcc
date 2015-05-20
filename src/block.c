#include "graph.c"

#define OP_FRAME_PUSH       1
#define OP_FRAME_POP        2
#define OP_SEAL             3
#define OP_UNSEAL           4
#define OP_ASSERT_EQUAL     5
#define OP_DEBUG_PRINT_RAW  6
#define OP_DEBUG_PRINT_TEXT 7

typedef struct AOStackFrame {
	struct AOStackFrame *next;
	StringRC *word;
	StringRC *file;
	u32 line;
	u32 refcount;
} AOStackFrame;
DEFINE_ARRAY(AOStackFrame *, AOStackFramePtr);

typedef struct Block {
	usize size;
	u8 *opcodes;
	Type **types;
	AOStackFrame **frames;
	struct Block **blocks;
	StringRC **texts;
	StringRC **sealers;
	BlockGraph graph;
	// Unlike for other structs, this is not used for memory management
	u32 refcount;
} Block;
DEFINE_ARRAY(Block *, BlockPtr);

internal
void print_backtrace(AOStackFrame *frame) {
	printf("Backtrace:\n");
	if (frame == NULL) {
		printf("No backtrace data available\n");
		return;
	}
	while (frame != NULL) {
		print_string(stdout, frame->file);
		printf(":%u: ", frame->line);
		print_string(stdout, frame->word);
		putchar('\n');
		frame = frame->next;
	}
}

internal
void ao_stack_frame_free(AOStackFrame frame) {
	string_rc_decref(frame.word);
	string_rc_decref(frame.file);
}

internal
void ao_stack_frame_decref(AOStackFrame *frame) {
	if (--frame->refcount == 0) {
		AOStackFrame *next = frame->next;
		ao_stack_frame_free(*frame);
		free(frame);
		if (next) {
			ao_stack_frame_decref(next);
		}
	}
}

internal
void block_free(Block *block) {
	for (usize i = 0, frame_index = 0, text_index = 0, sealer_index = 0; i < block->size; i++) {
		u8 opcode = block->opcodes[i];
		if (opcode == OP_FRAME_PUSH) {
			ao_stack_frame_decref(block->frames[frame_index++]);
		}
		if (opcode == '"') {
			string_rc_decref(block->texts[text_index++]);
		}
		if (opcode == OP_SEAL || opcode == OP_UNSEAL) {
			string_rc_decref(block->sealers[sealer_index++]);
		}
	}
	array_free(&block->opcodes);
	array_free(&block->types);
	array_free(&block->frames);
	array_free(&block->blocks);
	array_free(&block->texts);
	array_free(&block->sealers);
}
