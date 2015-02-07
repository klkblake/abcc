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
		// XXX This causes the buffer to crawl forward in memory. This
		// should be a big deal, but I can't get it to actually cause
		// problems, nor to do any better with smaller factors.
		slice->cap = slice->cap * 2;
		slice->data = realloc(slice->data, slice->cap * size);
	}
}

#define slice_trim(slice) slice_trim_((struct u8_slice *) (slice), sizeof((slice)->data[0]))
void slice_trim_(struct u8_slice *slice, usize size) {
	if (slice->cap) {
		slice->cap = slice->size;
		slice->data = realloc(slice->data, slice->cap * size);
	}
}

#define slice_bump(slice) slice_bump_((struct u8_slice *) (slice), sizeof((slice)->data[0]))
void *slice_bump_(struct u8_slice *slice, usize size) {
	if (slice->size == slice->cap) {
		slice_grow_(slice, size);
	}
	slice->size++;
	return &slice->data[(slice->size - 1) * size];
}

#define slice_snoc(slice, elem) ({ \
		typeof(slice) _slice = (slice); \
		*((typeof(_slice->data))slice_bump(_slice)) = elem; \
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

u32 jenkins_add(u32 hash, u8 *data, usize size) {
	for (usize i = 0; i < size; i++) {
		hash = jenkins_step(hash, data[i]);
	}
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
	return jenkins_finalise(jenkins_add(0, data, size));
}

#define DEFINE_MEMO_TABLE_STRUCT(name, type) \
struct name ## _memo_table { \
	u32 *hashes; \
	type **buckets; \
	usize size; \
	usize num_buckets; /* Must be power of two */ \
};
DEFINE_MEMO_TABLE_STRUCT(void, void);

void memo_table_maybe_grow(struct void_memo_table *table) {
	assert(table->size <= table->num_buckets);
	if (table->num_buckets == 0) {
		table->num_buckets = 64;
		table->hashes = malloc(table->num_buckets * sizeof(u32));
		table->buckets = malloc(table->num_buckets * sizeof(void *));
		memset(table->hashes, 0, table->num_buckets * sizeof(u32));
	}
	// Guarentee at least 50% free space for better performance
	if (table->size > table->num_buckets / 2) {
		usize new_num_buckets = table->num_buckets * 2;
		usize new_bucket_mask = new_num_buckets - 1;
		u32 *new_hashes = malloc(new_num_buckets * sizeof(u32));
		void **new_buckets = malloc(new_num_buckets * sizeof(void *));
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
	}
}

#define ESCAPE_COMMAS(...) __VA_ARGS__
#define DEFINE_MEMOISE(name, type, args, hash_expr, equal_expr, decref_internals_expr, malloc_expr, init_expr) \
type *memoise_ ## name(struct name ## _memo_table *table, args) { \
	u32 hash = (hash_expr); \
	memo_table_maybe_grow((struct void_memo_table *) table); \
	usize bucket_mask = table->num_buckets - 1; \
	usize bucket = hash & bucket_mask; \
	while (table->hashes[bucket] != 0) { \
		if (table->hashes[bucket] == hash) { \
			type *entry = table->buckets[bucket]; \
			if (equal_expr) { \
				entry->refcount++; \
				decref_internals_expr; \
				return entry; \
			} \
		} \
		bucket = (bucket + 1) & bucket_mask; \
	} \
	type *entry = malloc(malloc_expr); \
	init_expr; \
	entry->refcount = 2; /* The table retains a reference to the entry */ \
	table->hashes[bucket] = hash; \
	table->buckets[bucket] = entry; \
	table->size++; \
	return entry; \
}

#define DEFINE_MEMO_TABLE_FREE(name) \
void name ## _memo_table_free(struct name ## _memo_table *table) { \
	for (usize i = 0; i < table->num_buckets; i++) { \
		if (table->hashes[i] != 0) { \
			name ## _decref(table->buckets[i]); \
		} \
	} \
	free(table->hashes); \
	free(table->buckets); \
}

#define DEFINE_MEMO_TABLE(name, type, args, hash_expr, equal_expr, decref_internals_expr, malloc_expr, init_expr) \
	DEFINE_MEMO_TABLE_STRUCT(name, type); \
	DEFINE_MEMOISE(name, type, ESCAPE_COMMAS(args), hash_expr, equal_expr, decref_internals_expr, malloc_expr, init_expr); \
	DEFINE_MEMO_TABLE_FREE(name);

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

DEFINE_MEMO_TABLE(string_rc, struct string_rc,
                  ESCAPE_COMMAS(u8 *data, usize size),
                  jenkins_hash(data, size),
                  entry->size == size && memcmp(entry->data, data, size) == 0,
                  ,
                  sizeof(struct string_rc) + size * sizeof(u8),
                  { entry->size = size; memcpy(entry->data, data, size); });

#define OP_FRAME_PUSH       1
#define OP_FRAME_POP        2
#define OP_SEAL             3
#define OP_UNSEAL           4
#define OP_ASSERT_EQUAL     5
#define OP_DEBUG_PRINT_RAW  6
#define OP_DEBUG_PRINT_TEXT 7

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

DEFINE_MEMO_TABLE(ao_stack_frame, struct ao_stack_frame,
                  ESCAPE_COMMAS(struct ao_stack_frame *next, struct string_rc *word, struct string_rc *file, u32 line),
                  ({
                   u32 hash = 0;
		   hash = jenkins_add(hash, (u8 *)&next, sizeof(next));
		   hash = jenkins_add(hash, (u8 *)&word, sizeof(word));
		   hash = jenkins_add(hash, (u8 *)&file, sizeof(file));
		   hash = jenkins_add(hash, (u8 *)&line, sizeof(line));
		   jenkins_finalise(hash);
                   }),
                  entry->next == next && entry->word == word && entry->file == file && entry->line == line,
                  { string_rc_decref(word); string_rc_decref(file); },
                  sizeof(struct ao_stack_frame),
                  ({ *entry = (struct ao_stack_frame){ next, word, file, line, 0 }; if (next) { next->refcount++; } }));

struct block {
	usize size;
	u8 *opcodes;
	struct ao_stack_frame **frames;
	struct block **blocks;
	struct string_rc **texts;
	struct string_rc **sealers;
	// Unlike for other structs, this is not used for memory management
	u32 refcount;
};
DEFINE_SLICE(struct block, block);
DEFINE_SLICE(struct block *, block_ptr);

struct incomplete_block {
	struct u8_slice opcodes;
	struct ao_stack_frame_ptr_slice frames;
	struct block_ptr_slice blocks;
	struct string_rc_ptr_slice texts;
	struct string_rc_ptr_slice sealers;
};

b32 blocks_equal(struct block *a, struct block *b) {
	if (a->size != b->size) {
		return false;
	}
	for (usize i = 0, frame_index = 0, block_index = 0, text_index = 0, sealer_index = 0; i < a->size; i++) {
		if (a->opcodes[i] != b->opcodes[i]) {
			return false;
		}
		u8 opcode = a->opcodes[i];
		if (opcode == OP_FRAME_PUSH) {
			if (a->frames[frame_index] != b->frames[frame_index]) {
				return false;
			}
			frame_index++;
		}
		if (opcode == '[') {
			if (a->blocks[block_index] != b->blocks[block_index]) {
				return false;
			}
			block_index++;
		}
		if (opcode == '"') {
			if (a->texts[text_index] != b->texts[text_index]) {
				return false;
			}
			text_index++;
		}
		if (opcode == OP_SEAL || opcode == OP_UNSEAL) {
			if (a->sealers[sealer_index] != b->sealers[sealer_index]) {
				return false;
			}
			sealer_index++;
		}
	}
	return true;
}

void block_decref(struct block *block) {
	block->refcount--;
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
	slice_free(&block->opcodes);
	slice_free(&block->frames);
	slice_free(&block->blocks);
	slice_free(&block->texts);
	slice_free(&block->sealers);
}

void block_clear(struct block *block) {
	block_free(block);
	*block = (struct block){};
}

DEFINE_MEMO_TABLE(block, struct block,
                  ESCAPE_COMMAS(struct block *block, u32 bhash),
                  bhash,
                  blocks_equal(entry, block),
                  { block_free(block); block->refcount = 0; },
                  sizeof(struct block),
                  *entry = *block);

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
	struct incomplete_block *block;
	u32 hash; // Hash of all parsed chars (excluding SP and LF), Jenkins One-at-a-Time hash.
	struct parse_error_slice errors;
	struct block_ptr_slice blocks;
	struct string_rc_memo_table string_table;
	struct ao_stack_frame_memo_table frame_table;
	struct block_memo_table block_table;
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
		state->hash = jenkins_step(state->hash, c);
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

b32 eat_unknown_annotation(struct parse_state *state) {
	report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
	i32 c;
	while ((c = next(state)) != '}') {
		if (c == EOF) {
			return false;
		}
	}
	return true;
}

b32 parse_stack_annotation(struct parse_state *state, b32 enter) {
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
		struct string_rc *word = memoise_string_rc(&state->string_table, word_slice.data, word_slice.size);
		struct string_rc *file = memoise_string_rc(&state->string_table, file_slice.data, file_slice.size);
		slice_stack_free(word_buf, &word_slice);
		slice_stack_free(file_buf, &file_slice);
		struct ao_stack_frame *frame = memoise_ao_stack_frame(&state->frame_table, state->frame, word, file, line);
		state->frame = frame;
		slice_snoc(&state->block->opcodes, OP_FRAME_PUSH);
		slice_snoc(&state->block->frames, frame);
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
			// TODO Should we pop the frame?
			report_error_here(state, PARSE_WARN_MISMATCHED_FRAME_POP);
			slice_stack_free(word_buf, &word_slice);
			slice_stack_free(file_buf, &file_slice);
			return true;
		}
		slice_stack_free(word_buf, &word_slice);
		slice_stack_free(file_buf, &file_slice);
		state->frame = old->next;
		slice_snoc(&state->block->opcodes, OP_FRAME_POP);
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

b32 parse_annotation(struct parse_state *state) {
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
		slice_snoc(&state->block->opcodes, OP_ASSERT_EQUAL);
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
		slice_snoc(&state->block->opcodes, op);
	} else {
		return eat_unknown_annotation(state);
	}
	return true;
}

b32 parse_sealer(struct parse_state *state, u8 op) {
	u8 buf[4096];
	struct u8_slice text = {buf, 0, sizeof(buf) / sizeof(u8)};
	i32 c;
	while ((c = next(state)) != '}') {
		if (c == EOF) {
			return false;
		}
		slice_stack_snoc(buf, &text, c);
	}
	slice_snoc(&state->block->opcodes, op);
	slice_snoc(&state->block->sealers, memoise_string_rc(&state->string_table, text.data, text.size));
	slice_stack_free(buf, &text);
	return true;
}

b32 parse_invokation(struct parse_state *state) {
	i32 type = next(state);
	if (type == EOF) {
		report_error_here(state, PARSE_ERR_EOF_IN_INVOCATION);
		return false;
	}
	b32 succeeded;
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

b32 parse_text(struct parse_state *state) {
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
	slice_snoc(&state->block->opcodes, '"');
	slice_snoc(&state->block->texts, memoise_string_rc(&state->string_table, text.data, text.size));
	slice_stack_free(buf, &text);
	return true;
}

struct block *parse_block(struct parse_state *state, b32 expect_eof) {
	struct incomplete_block block = {};
	state->frame = NULL;
	state->block = &block;
	state->hash = 0;
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
			return NULL;
		}
		if (c == EOF || c == ']') {
			slice_trim(&block.opcodes);
			slice_trim(&block.frames);
			slice_trim(&block.blocks);
			slice_trim(&block.texts);
			slice_trim(&block.sealers);
			struct block complete_block = {
				block.opcodes.size,
				block.opcodes.data,
				block.frames.data,
				block.blocks.data,
				block.texts.data,
				block.sealers.data,
				1,
			};
			struct block *memo_block = memoise_block(&state->block_table, &complete_block, jenkins_finalise(state->hash));
			if (complete_block.refcount) {
				// This field is unused, so we have the memo
				// table implementation clear it to signal that
				// a matching block was found.
				slice_snoc(&state->blocks, memo_block);
			}
			return memo_block;
		}
		struct ao_stack_frame *frame;
		b32 succeeded;
		switch ((u8)c) {
			case '[':
				frame = state->frame;
				u32 hash = state->hash;
				u32 line = state->line;
				u32 col = state->col;
				struct block *new_block = parse_block(state, false);
				if (new_block == NULL) {
					return NULL;
				}
				state->frame = frame;
				state->block = &block;
				state->hash = jenkins_add(hash, (u8 *)&state->hash, sizeof(state->hash));
				if (next(state) != ']') {
					report_error(state, PARSE_ERR_EOF_IN_BLOCK, line, col);
					return NULL;
				}
				slice_snoc(&state->block->opcodes, '[');
				slice_snoc(&block.blocks, new_block);
				break;
			case '{':
				succeeded = parse_invokation(state);
				if (!succeeded) {
					return NULL;
				}
				break;
			case '"':
				succeeded = parse_text(state);
				if (!succeeded) {
					return NULL;
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
				slice_snoc(&state->block->opcodes, c);
				break;

			default:
				report_error_here(state, PARSE_ERR_ILLEGAL_OPCODE);
				return NULL;
		}
	}
}

struct parse_result {
	// NULL if the parse failed
	struct block *block;
	// All the blocks transitively referenced by block, sorted
	// topologically (leaves first)
	struct block_ptr_slice blocks;
	struct parse_error_slice errors;
};

struct parse_result parse(FILE *stream) {
	struct parse_state state = {};
	state.stream = stream;
	struct block *block = parse_block(&state, true);
	if (state.frame != NULL) {
		report_error_here(&state, PARSE_WARN_EOF_IN_FRAME);
		ao_stack_frame_decref(state.frame);
	}
	string_rc_memo_table_free(&state.string_table);
	ao_stack_frame_memo_table_free(&state.frame_table);
	block_memo_table_free(&state.block_table);
	slice_trim(&state.blocks);
	return (struct parse_result){block, state.blocks, state.errors};
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
	struct parse_result result = parse(stdin);
	foreach (error, result.errors) {
		print_parse_error(*error);
	}
	slice_free(&result.errors);
	if (!result.block) {
		printf("Parse failed.\n");
		return 1;
	}
	printf("Parse succeeded. %zu blocks.\n", result.blocks.size);

	foreach (block, result.blocks) {
		block_free(*block);
	}
	slice_free(&result.blocks);
	return 0;
}
