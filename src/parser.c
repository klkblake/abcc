#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "types.h"

struct u8_slice;
void slice_grow_(struct u8_slice *slice, usize size);

#define DEFINE_SLICE(type, name) \
	struct name ## _slice { \
		type *data; \
		usize size; \
		usize cap; \
	};

DEFINE_SLICE(u8, u8);
DEFINE_SLICE(u8 *, u8_ptr);
DEFINE_SLICE(u32, u32);

#define slice_grow(slice) slice_grow_((struct u8_slice *) (slice), sizeof((slice)->data[0]))
void slice_grow_(struct u8_slice *slice, usize size) {
	if (slice->cap == 0) {
		slice->cap = 64;
		slice->data = malloc(slice->cap * size);
	} else {
		slice->cap *= 2;
		slice->data = realloc(slice->data, slice->cap * size);
	}
}

#define slice_trim(slice) slice_trim_((struct u8_slice *) (slice), sizeof((slice)->data[0]))
void slice_trim_(struct u8_slice *slice, usize size) {
	slice->cap = slice->size;
	slice->data = realloc(slice->data, slice->cap * size);
}

#define slice_bump(slice) slice_bump_((struct u8_slice *) (slice), sizeof((slice)->data[0]))
void slice_bump_(struct u8_slice *slice, usize size) {
	if (slice->size == slice->cap) {
		slice_grow_(slice, size);
	}
	slice->size++;
}

#define slice_snoc(slice, elem) ({ \
		typeof(slice) _slice = (slice); \
		slice_bump(_slice); \
		_slice->data[_slice->size - 1] = elem; \
	})

void slice_free(void *slice) {
	struct u8_slice *slice2 = slice;
	free(slice2->data);
}

void slice_clear(void *slice) {
	slice_free(slice);
	*((struct u8_slice *)slice) = (struct u8_slice){};
}

void slice_stack_snoc(u8 *buf, struct u8_slice *slice, u8 c) {
	if (slice->size == slice->cap) {
		if (slice->data == buf) {
			usize newcap = slice->cap * 2;
			u8 *newdata = malloc(newcap * sizeof(u8));
			memcpy(newdata, slice->data, slice->cap * sizeof(u8));
			slice->data = newdata;
			slice->cap = newcap;
		} else {
			slice_grow(slice);
		}
	}
	slice->data[slice->size++] = c;
}

void slice_stack_free(u8 *buf, struct u8_slice *slice) {
	if (slice->data != buf) {
		slice_free(slice);
	}
}

#define foreach(var, slice) \
	for (usize var ## _index = 0; var ## _index != (usize)-1; var ## _index = -1) \
		for (typeof((slice).data) var; \
		     var ## _index < (slice).size && (var = &(slice).data[var ## _index], true); \
		     var ## _index++)

u32 jenkins_step(u32 hash, u8 data) {
	hash = hash + data;
	hash += hash << 10;
	hash ^= hash >> 6;
	return hash;
}

u32 jenkins_finalise(u32 hash) {
	hash += hash << 3;
	hash ^= hash >> 11;
	hash += hash << 15;
	// We use 0 to indicate no entry
	if (hash == 0) {
		hash = 1 << 31;
	}
	return hash;
}

u32 jenkins_hash(u8 *data, usize size) {
	u32 hash = 0;
	for (usize i = 0; i < size; i++) {
		hash = jenkins_step(hash, data[i]);
	}
	return jenkins_finalise(hash);
}

struct string_rc {
	usize size;
	u32 refcount;
	u8 data[];
};
DEFINE_SLICE(struct string_rc *, string_rc_ptr);

void string_rc_decref(struct string_rc *str) {
	if (--str->refcount == 0) {
		free(str);
	}
}

struct string_memo_table {
	u32 *hashes;
	struct string_rc **buckets;
	usize size;
	usize num_buckets; // Must be power of two
};

struct string_rc *memoise_string(struct string_memo_table *table, u8 *data, usize size) {
	u32 hash = jenkins_hash(data, size);
	if (table->num_buckets == 0) {
		table->num_buckets = 64;
		table->hashes = malloc(table->num_buckets * sizeof(u32));
		table->buckets = malloc(table->num_buckets * sizeof(struct string_rc *));
		memset(table->hashes, 0, table->num_buckets * sizeof(u32));
	}
	usize bucket_mask = table->num_buckets - 1;
	if (table->size > table->num_buckets / 2) {
		usize new_num_buckets = table->num_buckets * 2;
		usize new_bucket_mask = new_num_buckets - 1;
		u32 *new_hashes = malloc(new_num_buckets * sizeof(u32));
		struct string_rc **new_buckets = malloc(new_num_buckets * sizeof(struct string_rc *));
		memset(new_hashes, 0, new_num_buckets * sizeof(u32));
		for (usize i = 0; i < table->num_buckets; i++) {
			u32 new_hash = table->hashes[i];
			if (new_hash == 0) {
				continue;
			}
			u32 new_bucket = new_hash & new_bucket_mask;
			while (new_hashes[new_bucket] != 0) {
				new_bucket = (new_bucket + 1) & new_bucket_mask;
			}
			new_hashes[new_bucket] = new_hash;
			new_buckets[new_bucket] = table->buckets[i];
		}
		free(table->hashes);
		free(table->buckets);
		table->num_buckets = new_num_buckets;
		table->hashes = new_hashes;
		table->buckets = new_buckets;
		bucket_mask = new_bucket_mask;
	}
	usize bucket = hash & bucket_mask;
	while (table->hashes[bucket] != 0) {
		if (table->hashes[bucket] == hash) {
			struct string_rc *str = table->buckets[bucket];
			if (str->size == size && memcmp(str->data, data, size) == 0) {
				str->refcount++;
				return str;
			}
		}
		bucket = (bucket + 1) & bucket_mask;
	}
	struct string_rc *str = malloc(sizeof(struct string_rc) + size * sizeof(u8));
	str->size = size;
	str->refcount = 2; // The table retains a reference to the string
	memcpy(str->data, data, size);
	table->hashes[bucket] = hash;
	table->buckets[bucket] = str;
	table->size++;
	return str;
}

void string_memo_table_free(struct string_memo_table *table) {
	for (usize i = 0; i < table->num_buckets; i++) {
		if (table->hashes[i] != 0) {
			string_rc_decref(table->buckets[i]);
		}
	}
	free(table->hashes);
	free(table->buckets);
}

#define OP_SEAL             1
#define OP_UNSEAL           2
#define OP_ASSERT_EQUAL     3
#define OP_DEBUG_PRINT_RAW  4
#define OP_DEBUG_PRINT_TEXT 5

struct ao_stack_frame {
	struct ao_stack_frame *next;
	struct string_rc *word;
	struct string_rc *file;
	u32 line;
	u32 refcount;
};
DEFINE_SLICE(struct ao_stack_frame *, ao_stack_frame_ptr);

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

struct block {
	u32 hash; // Hash of opcode values, Jenkins One-at-a-Time hash.
	struct u8_slice opcodes;
	struct ao_stack_frame_ptr_slice frames;
	struct u32_slice blocks;
	struct string_rc_ptr_slice texts;
	struct string_rc_ptr_slice sealers;
};
DEFINE_SLICE(struct block, block);

void block_free(struct block *block) {
	slice_free(&block->opcodes);
	foreach (frame, block->frames) {
		if (*frame) {
			ao_stack_frame_decref(*frame);
		}
	}
	slice_free(&block->frames);
	slice_free(&block->blocks);
	foreach (text, block->texts) {
		string_rc_decref(*text);
	}
	slice_free(&block->texts);
	foreach (sealer, block->sealers) {
		string_rc_decref(*sealer);
	}
	slice_free(&block->sealers);
}

void block_clear(struct block *block) {
	block_free(block);
	*block = (struct block){};
}

#define PARSE_WARN (1 << 31)
#define PARSE_ERR_EOF_IN_BLOCK        0
#define PARSE_ERR_EOF_IN_INVOCATION   1
#define PARSE_ERR_EOF_IN_TEXT         2
#define PARSE_ERR_ILLEGAL_OPCODE      3
#define PARSE_ERR_UNEXPECTED_BRACKET  4
#define PARSE_ERR_UNKNOWN_INVOCATION  5
#define PARSE_ERR_MALFORMED_TEXT      6
#define PARSE_WARN_UNKNOWN_ANNOTATION   (PARSE_WARN | 0)
#define PARSE_WARN_UNEXPECTED_FRAME_POP (PARSE_WARN | 1)
#define PARSE_WARN_MISMATCHED_FRAME_POP (PARSE_WARN | 2)
#define PARSE_WARN_EOF_IN_FRAME         (PARSE_WARN | 3)

char *parse_error_messages[] = {
	"Hit end-of-file while parsing block",
	"Hit end-of-file while parsing invocation",
	"Hit end-of-file while parsing text",
	"Illegal opcode",
	"Unexpected closing bracket ']'",
	"Unknown invocation",
	"Malformed text literal",
};

char *parse_warning_messages[] = {
	"Unknown annotation",
	"Popped frame when already at top level",
	"Tried to pop frame that does not match the top frame",
	"Hit end-of-file while inside stack frame",
};

struct parse_error {
	u32 code;
	u32 line;
	u32 col;
};
DEFINE_SLICE(struct parse_error, parse_error);

struct parse_state {
	FILE *stream;
	i32 line;
	i32 col;
	struct ao_stack_frame *frame;
	struct block block;
	struct parse_error_slice errors;
	struct block_slice blocks;
	struct string_memo_table memo_table;
};

i32 next(struct parse_state *state) {
	int c = getc(state->stream);
	// Don't increment the column for UTF-8 continuation characters.
	// TODO Proper column/line handling for arbitrary UTF-8 text
	if (c < 0 || ((u8)c) >> 6 != 2) {
		state->col++;
	}
	if (c == '\n') {
		state->line++;
		state->col = 0;
	}
	if (c != ' ' && c != '\n') {
		state->block.hash = jenkins_step(state->block.hash, c);
	}
	return c;
}

void report_error(struct parse_state *state, u32 code, u32 line, u32 col) {
	struct parse_error error = {
		code,
		line,
		col,
	};
	slice_snoc(&state->errors, error);
}

void report_error_here(struct parse_state *state, u32 code) {
	report_error(state, code, state->line, state->col);
}

void snoc_opcode(struct parse_state *state, u8 opcode) {
	slice_snoc(&state->block.opcodes, opcode);
	slice_snoc(&state->block.frames, state->frame);
	if (state->frame) {
		state->frame->refcount++;
	}
}

i32 eat_unknown_annotation(struct parse_state *state) {
	report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
	i32 c;
	while ((c = next(state)) != '}') {
		if (c == EOF) {
			return false;
		}
	}
	return true;
}

b1 parse_stack_annotation(struct parse_state *state, b1 enter) {
	i32 c;
	u8 word_buf[4096];
	struct u8_slice word_slice = {word_buf, 0, sizeof(word_buf)/sizeof(u8)};
	while ((c = next(state)) != '@') {
		if (c == '}') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			return true;
		}
		if (c == EOF) {
			return false;
		}
		slice_stack_snoc(word_buf, &word_slice, c);
	}
	u8 file_buf[4096];
	struct u8_slice file_slice = (struct u8_slice){file_buf, 0, sizeof(file_buf)/sizeof(u8)};
	while ((c = next(state)) != ':') {
		if (c == '}') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			slice_stack_free(word_buf, &word_slice);
			return true;
		}
		if (c == EOF) {
			slice_stack_free(word_buf, &word_slice);
			return false;
		}
		slice_stack_snoc(file_buf, &file_slice, c);
	}
	u32 line = 0;
	while ((c = next(state)) != '}') {
		if (c < '0' || c > '9') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			slice_stack_free(word_buf, &word_slice);
			slice_stack_free(file_buf, &file_slice);
			return true;
		}
		if (c == EOF) {
			slice_stack_free(word_buf, &word_slice);
			slice_stack_free(file_buf, &file_slice);
			return false;
		}
		line = line * 10 + c - '0';
	}
	if (enter) {
		struct ao_stack_frame *frame = malloc(sizeof(struct ao_stack_frame));
		struct string_rc *word = memoise_string(&state->memo_table, word_slice.data, word_slice.size);
		struct string_rc *file = memoise_string(&state->memo_table, file_slice.data, file_slice.size);
		slice_stack_free(word_buf, &word_slice);
		slice_stack_free(file_buf, &file_slice);
		*frame = (struct ao_stack_frame){state->frame, word, file, line, 1};
		if (state->frame) {
			state->frame->refcount++;
		}
		state->frame = frame;
	} else {
		struct ao_stack_frame *old = state->frame;
		if (old == NULL) {
			report_error_here(state, PARSE_WARN_UNEXPECTED_FRAME_POP);
			slice_stack_free(word_buf, &word_slice);
			slice_stack_free(file_buf, &file_slice);
			return true;
		}
		if (old->word->size != word_slice.size ||
		    old->file->size != file_slice.size ||
		    memcmp(old->word->data, word_slice.data, word_slice.size) != 0 ||
		    memcmp(old->file->data, file_slice.data, file_slice.size) != 0 ||
		    old->line != line) {
			report_error_here(state, PARSE_WARN_MISMATCHED_FRAME_POP);
			slice_stack_free(word_buf, &word_slice);
			slice_stack_free(file_buf, &file_slice);
			return true;
		}
		slice_stack_free(word_buf, &word_slice);
		slice_stack_free(file_buf, &file_slice);
		state->frame = old->next;
		ao_stack_frame_decref(old);
	}
	return true;
}

// 0 on success, 1 on failure, EOF on EOF
i32 match_annotation_part(struct parse_state *state, u8 *target) {
	while (*target != '\0') {
		i32 c = next(state);
		if (c == EOF) {
			return EOF;
		}
		if (c != *target) {
			if (!eat_unknown_annotation(state)) {
				return EOF;
			}
			return 1;
		}
	}
	return 0;
}

b1 parse_annotation(struct parse_state *state) {
	u8 cs[4];
	i32 c = next(state);
	if (c == '}') {
		report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
		return true;
	}
	if (c == EOF) {
		return false;
	}
	if (c == '+') {
		return parse_stack_annotation(state, true);
	} else {
		return parse_stack_annotation(state, false);
	}
	cs[0] = c;
	for (i32 i = 1; i < 4; i++) {
		c = next(state);
		if (c == '}') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			return true;
		}
		if (c == EOF) {
			return false;
		}
		cs[i] = c;
	}
	// UTF-8 encoding of ≡
	if (cs[0] == 0xe2 && cs[1] ==0x89 && cs[2] == 0xa1 && cs[3] == '}') {
		snoc_opcode(state, OP_ASSERT_EQUAL);
	} else if (cs[0] == 'd' && cs[1] == 'e' && cs[2] == 'b' && cs[3] == 'u') {
		i32 m = match_annotation_part(state, (u8 *)"g print ");
		if (m) {
			return m != EOF;
		}
		c = next(state);
		if (c == '}') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			return true;
		}
		if (c == EOF) {
			return false;
		}
		u8 *target;
		u8 op;
		if (c == 'r') {
			target = (u8 *) "aw}";
			op = OP_DEBUG_PRINT_RAW;
		} else if (c == 't') {
			target = (u8 *) "ext}";
			op = OP_DEBUG_PRINT_TEXT;
		} else {
			return eat_unknown_annotation(state);
		}
		m = match_annotation_part(state, target);
		if (m) {
			return m != EOF;
		}
		snoc_opcode(state, op);
	} else {
		return eat_unknown_annotation(state);
	}
	return true;
}

b1 parse_sealer(struct parse_state *state, u8 op) {
	u8 buf[4096];
	struct u8_slice text = {buf, 0, sizeof(buf) / sizeof(u8)};
	i32 c;
	while ((c = next(state)) != '}') {
		if (c == EOF) {
			return false;
		}
		slice_stack_snoc(buf, &text, c);
	}
	snoc_opcode(state, op);
	slice_snoc(&state->block.sealers, memoise_string(&state->memo_table, text.data, text.size));
	slice_stack_free(buf, &text);
	return true;
}

b1 parse_invokation(struct parse_state *state) {
	i32 type = next(state);
	if (type == EOF) {
		report_error_here(state, PARSE_ERR_EOF_IN_INVOCATION);
		return false;
	}
	b1 succeeded;
	switch ((u8)type) {
		case '&':
			succeeded = parse_annotation(state);
			break;
		case ':':
			succeeded = parse_sealer(state, OP_SEAL);
			break;
		case '.':
			succeeded = parse_sealer(state, OP_UNSEAL);
			break;
		default:
			report_error_here(state, PARSE_ERR_UNKNOWN_INVOCATION);
			return false;
	}
	if (!succeeded) {
		report_error_here(state, PARSE_ERR_EOF_IN_INVOCATION);
	}
	return succeeded;
}

b1 parse_text(struct parse_state *state) {
	u32 line = state->line;
	u32 col = state->col;
	u8 buf[4096];
	struct u8_slice text = {buf, 0, sizeof(buf) / sizeof(u8)};
	while (true) {
		i32 c = next(state);
		if (c == EOF) {
			report_error(state, PARSE_ERR_EOF_IN_TEXT, line, col);
			return false;
		}
		if (c == '\n') {
			i32 c2 = next(state);
			if (c2 == EOF) {
				report_error(state, PARSE_ERR_EOF_IN_TEXT, line, col);
				return false;
			}
			if (c2 == '~') {
				break;
			}
			if (c2 != ' ') {
				report_error_here(state, PARSE_ERR_MALFORMED_TEXT);
				return false;
			}
		}
		slice_stack_snoc(buf, &text, c);
	}
	snoc_opcode(state, '"');
	slice_snoc(&state->block.texts, memoise_string(&state->memo_table, text.data, text.size));
	slice_stack_free(buf, &text);
	return true;
}

b1 parse_block(struct parse_state *state, b1 expect_eof) {
	state->frame = NULL;
	state->block = (struct block){};
	while (true) {
		i32 c = next(state);
		if (c == ' ' || c == '\n') {
			continue;
		}
		if (c == ']') {
			int err = ungetc(']', state->stream);
			assert(err != EOF);
		}
		if (c == ']' && expect_eof) {
			report_error_here(state, PARSE_ERR_UNEXPECTED_BRACKET);
			return false;
		}
		if (c == EOF || c == ']') {
			slice_trim(&state->block.opcodes);
			slice_trim(&state->block.blocks);
			slice_trim(&state->block.texts);
			slice_trim(&state->block.sealers);
			state->block.hash = jenkins_finalise(state->block.hash);
			slice_snoc(&state->blocks, state->block);
			return true;
		}
		struct ao_stack_frame *frame;
		struct block block;
		b1 succeeded;
		switch ((u8)c) {
			case '[':
				frame = state->frame;
				block = state->block;
				u32 line = state->line;
				u32 col = state->col;
				succeeded = parse_block(state, false);
				if (!succeeded) {
					return false;
				}
				state->frame = frame;
				state->block = block;
				if (next(state) != ']') {
					report_error(state, PARSE_ERR_EOF_IN_BLOCK, line, col);
					return false;
				}
				snoc_opcode(state, '[');
				slice_snoc(&state->block.blocks, state->blocks.size - 1);
				break;
			case '{':
				succeeded = parse_invokation(state);
				if (!succeeded) {
					return false;
				}
				break;
			case '"':
				succeeded = parse_text(state);
				if (!succeeded) {
					return false;
				}
				break;

			case 'l':
			case 'r':
			case 'w':
			case 'z':
			case 'v':
			case 'c':
			case '%':
			case '^':

			case '$':
			case 'o':
			case '\'':
			case 'k':
			case 'f':

			case '#':
			case '0' ... '9':

			case '+':
			case '*':
			case '/':
			case '-':
			case 'Q':

			case 'L':
			case 'R':
			case 'W':
			case 'Z':
			case 'V':
			case 'C':

			case '?':
			case 'D':
			case 'F':
			case 'M':
			case 'K':
			case '>':
				snoc_opcode(state, c);
				break;

			default:
				report_error_here(state, PARSE_ERR_ILLEGAL_OPCODE);
				return false;
		}
	}
}

// TODO replace most usages of b1 with b32
b1 parse(struct parse_state *state) {
	b1 succeeded = parse_block(state, true);
	if (state->frame != NULL) {
		report_error_here(state, PARSE_WARN_EOF_IN_FRAME);
		ao_stack_frame_decref(state->frame);
		state->frame = NULL;
	}
	state->block = (struct block){};
	return succeeded;
}

void print_parse_error(struct parse_error error) {
	if ((error.code & PARSE_WARN) == 0) {
		fprintf(stderr, "error:%d:%d: %s\n", error.line, error.col, parse_error_messages[error.code]);
	} else {
		fprintf(stderr, "warning:%d:%d: %s\n", error.line, error.col,
				parse_warning_messages[error.code &~ PARSE_WARN]);
	}
}

int main() {
	struct parse_state state = {};
	state.stream = stdin;
	b1 succeeded = parse(&state);
	foreach (error, state.errors) {
		print_parse_error(*error);
	}
	slice_clear(&state.errors);
	if (!succeeded) {
		printf("Parse failed.\n");
		return 1;
	}
	printf("Parse succeeded. %zu blocks.\n", state.blocks.size);

	string_memo_table_free(&state.memo_table);
	foreach (block, state.blocks) {
		block_free(block);
	}
	slice_clear(&state.blocks);
	// TODO free when done, only when checking for leaks
	return 0;
}
