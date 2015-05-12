#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#undef assert
#define assert(cond) if (!(cond)) { __asm__("int3"); __builtin_unreachable(); }

typedef __INT8_TYPE__  i8;
typedef __INT16_TYPE__ i16;
typedef __INT32_TYPE__ i32;
typedef __INT64_TYPE__ i64;

typedef __UINT8_TYPE__  u8;
typedef __UINT16_TYPE__ u16;
typedef __UINT32_TYPE__ u32;
typedef __UINT64_TYPE__ u64;

typedef double f64;

typedef _Bool b1;
typedef u32  b32;

typedef __SIZE_TYPE__  usize;

static_assert(sizeof(f64) == 8, "f64 is the wrong size");

#define internal static

#define offsetof(type, member) __builtin_offsetof(type, member)

#define array_count(array) (sizeof(array) / sizeof((array)[0]))
