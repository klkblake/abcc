#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "abcc.h"
#include "parser.h"

internal
void array_stack_snoc(u8 *buf, struct u8_array *array, u8 c) {
	if (array->size == array->cap) {
		if (array->data == buf) {
			usize newcap = array->cap * 2;
			u8 *newdata = malloc(newcap * sizeof(u8));
			memcpy(newdata, array->data, array->cap * sizeof(u8));
			array->data = newdata;
			array->cap = newcap;
		} else {
			array_grow(array);
		}
	}
	array->data[array->size++] = c;
}

internal
void array_stack_free(u8 *buf, struct u8_array *array) {
	if (array->data != buf) {
		array_free(array);
	}
}

internal
u32 jenkins_step(u32 hash, u8 data) {
	hash = hash + data;
	hash += hash << 10;
	hash ^= hash >> 6;
	return hash;
}

internal
u32 jenkins_add(u32 hash, u8 *data, usize size) {
	for (usize i = 0; i < size; i++) {
		hash = jenkins_step(hash, data[i]);
	}
	return hash;
}

internal
u32 jenkins_finalise(u32 hash) {
	hash += hash << 3;
	hash ^= hash >> 11;
	hash += hash << 15;
	// We use 0 to indicate no entry
	if (hash == 0) {
		hash = 1u << 31;
	}
	return hash;
}

internal
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

internal
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
internal \
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
internal \
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

DEFINE_ARRAY(struct string_rc *, string_rc_ptr);

DEFINE_MEMO_TABLE(string_rc, struct string_rc,
                  ESCAPE_COMMAS(u8 *data, usize size),
                  jenkins_hash(data, size),
                  entry->size == size && memcmp(entry->data, data, size) == 0,
                  ,
                  sizeof(struct string_rc) + size * sizeof(u8),
                  { entry->size = size; memcpy(entry->data, data, size); });

DEFINE_ARRAY(struct ao_stack_frame *, ao_stack_frame_ptr);

internal
u32 hash_ao_stack_frame(struct ao_stack_frame *next, struct string_rc *word, struct string_rc *file, u32 line) {
                   u32 hash = 0;
		   hash = jenkins_add(hash, (u8 *)&next, sizeof(next));
		   hash = jenkins_add(hash, (u8 *)&word, sizeof(word));
		   hash = jenkins_add(hash, (u8 *)&file, sizeof(file));
		   hash = jenkins_add(hash, (u8 *)&line, sizeof(line));
		   return jenkins_finalise(hash);
}

DEFINE_MEMO_TABLE(ao_stack_frame, struct ao_stack_frame,
                  ESCAPE_COMMAS(struct ao_stack_frame *next, struct string_rc *word, struct string_rc *file, u32 line),
                  hash_ao_stack_frame(next, word, file, line),
                  entry->next == next && entry->word == word && entry->file == file && entry->line == line,
                  { string_rc_decref(word); string_rc_decref(file); },
                  sizeof(struct ao_stack_frame),
                  ({ *entry = (struct ao_stack_frame){ next, word, file, line, 0 }; if (next) { next->refcount++; } }));

struct incomplete_block {
	struct u8_array opcodes;
	struct ao_stack_frame_ptr_array frames;
	struct block_ptr_array blocks;
	struct string_rc_ptr_array texts;
	struct string_rc_ptr_array sealers;
};

internal
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

internal
void block_decref(struct block *block) {
	block->refcount--;
}

DEFINE_MEMO_TABLE(block, struct block,
                  ESCAPE_COMMAS(struct block *block, u32 bhash),
                  bhash,
                  blocks_equal(entry, block),
                  { block_free(block); block->refcount = 0; },
                  sizeof(struct block),
                  *entry = *block);

#define PARSE_WARN (1u << 31)
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

static char *parse_error_messages[] = {
	"Hit end-of-file while parsing block",
	"Hit end-of-file while parsing invocation",
	"Hit end-of-file while parsing text",
	"Illegal opcode",
	"Unexpected closing bracket ']'",
	"Unknown invocation",
	"Malformed text literal",
};

static char *parse_warning_messages[] = {
	"Unknown annotation",
	"Popped frame when already at top level",
	"Tried to pop frame that does not match the top frame",
	"Hit end-of-file while inside stack frame",
};

struct parse_state {
	FILE *stream;
	u32 lineno;
	u32 col;
	struct u8_array line;
	u32 byte_index;
	struct ao_stack_frame *frame;
	struct incomplete_block *block;
	u32 hash; // Hash of all parsed chars (excluding SP and LF), Jenkins One-at-a-Time hash.
	struct parse_error_array errors;
	struct block_ptr_array blocks;
	struct string_rc_memo_table string_table;
	struct ao_stack_frame_memo_table frame_table;
	struct block_memo_table block_table;
};

internal
i32 next(struct parse_state *state) {
	if (state->byte_index == state->line.size) {
		usize oldsize = state->line.size;
		state->line.size = (usize)getline((char **)&state->line.data, &state->line.cap, state->stream);
		if (state->line.size == (usize)-1) {
			state->line.size = oldsize;
			return EOF;
		}
		state->byte_index = 0;
	}
	u8 c = state->line.data[state->byte_index++];
	// Don't increment the column for UTF-8 continuation characters.
	// TODO Proper column/line handling for arbitrary UTF-8 text
	if (c >> 6 != 2) {
		state->col++;
	}
	if (c == '\n') {
		state->lineno++;
		state->col = 0;
	}
	if (c != ' ' && c != '\n') {
		state->hash = jenkins_step(state->hash, c);
	}
	return c;
}

internal
void report_error(struct parse_state *state, u32 code, u32 lineno, u32 col) {
	struct u8_array line = {};
	if (lineno == state->lineno) {
		line.data = malloc(state->line.size);
		line.size = state->line.size;
		line.cap = line.size;
		memcpy(line.data, state->line.data, line.size);
	}
	struct parse_error error = {
		line,
		code,
		lineno,
		col,
	};
	array_push(&state->errors, error);
}

internal
void report_error_here(struct parse_state *state, u32 code) {
	report_error(state, code, state->lineno, state->col);
}

internal
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

internal
b32 parse_stack_annotation(struct parse_state *state, b32 enter) {
	i32 c;
	u8 word_buf[4096];
	struct u8_array word_array = {word_buf, 0, sizeof(word_buf)/sizeof(u8)};
	while ((c = next(state)) != '@') {
		if (c == '}') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			return true;
		}
		if (c == EOF) {
			return false;
		}
		array_stack_snoc(word_buf, &word_array, (u8)c);
	}
	u8 file_buf[4096];
	struct u8_array file_array = (struct u8_array){file_buf, 0, sizeof(file_buf)/sizeof(u8)};
	while ((c = next(state)) != ':') {
		if (c == '}') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			array_stack_free(word_buf, &word_array);
			return true;
		}
		if (c == EOF) {
			array_stack_free(word_buf, &word_array);
			return false;
		}
		array_stack_snoc(file_buf, &file_array, (u8)c);
	}
	u32 line = 0;
	while ((c = next(state)) != '}') {
		if (c < '0' || c > '9') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			array_stack_free(word_buf, &word_array);
			array_stack_free(file_buf, &file_array);
			return true;
		}
		if (c == EOF) {
			array_stack_free(word_buf, &word_array);
			array_stack_free(file_buf, &file_array);
			return false;
		}
		line = line * 10 + (u8)c - '0';
	}
	if (enter) {
		struct string_rc *word = memoise_string_rc(&state->string_table, word_array.data, word_array.size);
		struct string_rc *file = memoise_string_rc(&state->string_table, file_array.data, file_array.size);
		array_stack_free(word_buf, &word_array);
		array_stack_free(file_buf, &file_array);
		struct ao_stack_frame *frame = memoise_ao_stack_frame(&state->frame_table, state->frame, word, file, line);
		state->frame = frame;
		array_push(&state->block->opcodes, OP_FRAME_PUSH);
		array_push(&state->block->frames, frame);
	} else {
		struct ao_stack_frame *old = state->frame;
		if (old == NULL) {
			report_error_here(state, PARSE_WARN_UNEXPECTED_FRAME_POP);
			array_stack_free(word_buf, &word_array);
			array_stack_free(file_buf, &file_array);
			return true;
		}
		if (old->word->size != word_array.size ||
		    old->file->size != file_array.size ||
		    memcmp(old->word->data, word_array.data, word_array.size) != 0 ||
		    memcmp(old->file->data, file_array.data, file_array.size) != 0 ||
		    old->line != line) {
			// TODO Should we pop the frame?
			report_error_here(state, PARSE_WARN_MISMATCHED_FRAME_POP);
			array_stack_free(word_buf, &word_array);
			array_stack_free(file_buf, &file_array);
			return true;
		}
		array_stack_free(word_buf, &word_array);
		array_stack_free(file_buf, &file_array);
		state->frame = old->next;
		array_push(&state->block->opcodes, OP_FRAME_POP);
	}
	return true;
}

// 0 on success, 1 on failure, EOF on EOF
internal
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

internal
b32 parse_annotation(struct parse_state *state) {
	u8 cs[3];
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
	} else if (c == '-') {
		return parse_stack_annotation(state, false);
	}
	cs[0] = (u8)c;
	for (i32 i = 1; i < 3; i++) {
		c = next(state);
		if (c == '}') {
			report_error_here(state, PARSE_WARN_UNKNOWN_ANNOTATION);
			return true;
		}
		if (c == EOF) {
			return false;
		}
		cs[i] = (u8)c;
	}
	// UTF-8 encoding of ≡
	if (cs[0] == 0xe2 && cs[1] ==0x89 && cs[2] == 0xa1) {
		c = next(state);
		if (c != '}') {
			return eat_unknown_annotation(state);
		}
		array_push(&state->block->opcodes, OP_ASSERT_EQUAL);
	} else if (cs[0] == 'd' && cs[1] == 'e' && cs[2] == 'b') {
		i32 m = match_annotation_part(state, (u8 *)"ug print ");
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
		array_push(&state->block->opcodes, op);
	} else {
		return eat_unknown_annotation(state);
	}
	return true;
}

internal
b32 parse_sealer(struct parse_state *state, u8 op) {
	u8 buf[4096];
	struct u8_array text = {buf, 0, sizeof(buf) / sizeof(u8)};
	i32 c;
	while ((c = next(state)) != '}') {
		if (c == EOF) {
			return false;
		}
		array_stack_snoc(buf, &text, (u8)c);
	}
	array_push(&state->block->opcodes, op);
	array_push(&state->block->sealers, memoise_string_rc(&state->string_table, text.data, text.size));
	array_stack_free(buf, &text);
	return true;
}

internal
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

internal
b32 parse_text(struct parse_state *state) {
	u32 lineno = state->lineno;
	u32 col = state->col;
	u8 buf[4096];
	struct u8_array text = {buf, 0, sizeof(buf) / sizeof(u8)};
	while (true) {
		i32 c = next(state);
		if (c == EOF) {
			report_error(state, PARSE_ERR_EOF_IN_TEXT, lineno, col);
			return false;
		}
		if (c == '\n') {
			i32 c2 = next(state);
			if (c2 == EOF) {
				report_error(state, PARSE_ERR_EOF_IN_TEXT, lineno, col);
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
		array_stack_snoc(buf, &text, (u8)c);
	}
	array_push(&state->block->opcodes, '"');
	array_push(&state->block->texts, memoise_string_rc(&state->string_table, text.data, text.size));
	array_stack_free(buf, &text);
	return true;
}

struct parse_block_result {
	struct block *block;
	// Only set if block is non-null
	b32 eof;
};

internal
struct parse_block_result parse_block(struct parse_state *state, b32 expect_eof) {
	struct parse_block_result result = {};
	struct incomplete_block block = {};
	state->frame = NULL;
	state->block = &block;
	state->hash = 0;
	while (true) {
		i32 c = next(state);
		if (c == ' ' || c == '\n') {
			continue;
		}
		if (c == EOF) {
			result.eof = true;
		}
		if (c == ']' && expect_eof) {
			report_error_here(state, PARSE_ERR_UNEXPECTED_BRACKET);
			return result;
		}
		if (c == EOF || c == ']') {
			array_trim(&block.opcodes);
			array_trim(&block.frames);
			array_trim(&block.blocks);
			array_trim(&block.texts);
			array_trim(&block.sealers);
			struct block complete_block = {
				block.opcodes.size,
				block.opcodes.data,
				NULL,
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
				array_push(&state->blocks, memo_block);
			}
			result.block = memo_block;
			return result;
		}
		struct ao_stack_frame *frame;
		b32 succeeded;
		switch ((u8)c) {
			case '[':
				frame = state->frame;
				u32 hash = state->hash;
				u32 lineno = state->lineno;
				u32 col = state->col;
				struct parse_block_result res = parse_block(state, false);
				if (res.block == NULL) {
					return res;
				}
				state->frame = frame;
				state->block = &block;
				state->hash = jenkins_add(hash, (u8 *)&state->hash, sizeof(state->hash));
				if (res.eof) {
					report_error(state, PARSE_ERR_EOF_IN_BLOCK, lineno, col);
					return result;
				}
				array_push(&state->block->opcodes, '[');
				array_push(&block.blocks, res.block);
				break;
			case '{':
				succeeded = parse_invokation(state);
				if (!succeeded) {
					return result;
				}
				break;
			case '"':
				succeeded = parse_text(state);
				if (!succeeded) {
					return result;
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
				array_push(&state->block->opcodes, (u8)c);
				break;

			default:
				report_error_here(state, PARSE_ERR_ILLEGAL_OPCODE);
				return result;
		}
	}
}

struct parse_result parse(FILE *stream) {
	struct parse_state state = {};
	state.stream = stream;
	struct parse_block_result result = parse_block(&state, true);
	if (state.frame != NULL) {
		report_error_here(&state, PARSE_WARN_EOF_IN_FRAME);
		ao_stack_frame_decref(state.frame);
	}
	string_rc_memo_table_free(&state.string_table);
	ao_stack_frame_memo_table_free(&state.frame_table);
	block_memo_table_free(&state.block_table);
	array_trim(&state.blocks);
	array_free(&state.line);
	return (struct parse_result){result.block, state.blocks, state.errors};
}

void print_parse_error(struct parse_error error) {
	// We store line, col zero indexed instead of one indexed
	if ((error.code & PARSE_WARN) == 0) {
		fprintf(stderr, "error:%d:%d: %s\n", error.lineno + 1, error.col + 1, parse_error_messages[error.code]);
	} else {
		fprintf(stderr, "warning:%d:%d: %s\n", error.lineno + 1, error.col + 1,
				parse_warning_messages[error.code &~ PARSE_WARN]);
	}
	if (error.line.size != 0) {
		fwrite(error.line.data, 1, error.line.size, stderr);
		if (error.line.data[error.line.size - 1] != '\n') {
			fputc('\n', stderr);
		}
	}
}
