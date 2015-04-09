#include "block.h"

#include <stdlib.h>
#include <stdio.h>

#include "string.h"

void print_backtrace(struct ao_stack_frame *frame) {
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
void ao_stack_frame_free(struct ao_stack_frame frame) {
	string_rc_decref(frame.word);
	string_rc_decref(frame.file);
}

void ao_stack_frame_decref(struct ao_stack_frame *frame) {
	if (--frame->refcount == 0) {
		struct ao_stack_frame *next = frame->next;
		ao_stack_frame_free(*frame);
		free(frame);
		if (next) {
			ao_stack_frame_decref(next);
		}
	}
}

void block_free(struct block *block) {
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
