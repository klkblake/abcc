#ifndef ABCC_H

typedef __INT8_TYPE__  i8;
typedef __INT16_TYPE__ i16;
typedef __INT32_TYPE__ i32;
typedef __INT64_TYPE__ i64;

typedef __UINT8_TYPE__  u8;
typedef __UINT16_TYPE__ u16;
typedef __UINT32_TYPE__ u32;
typedef __UINT64_TYPE__ u64;

typedef _Bool b1;
typedef u32  b32;

typedef __SIZE_TYPE__  usize;

#define true  1
#define false 0
#define internal static
#define static_assert(cond, str) _Static_assert(cond, str)
#define assert(cond) if (cond) { __builtin_trap(); }
#define offsetof(type, member) __builtin_offsetof(type, member)

#define ABCC_H
#endif
