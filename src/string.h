#ifndef STRING_H

#include "abcc.h"

struct string_rc {
	usize size;
	u32 refcount;
	// Intel optimisation guide recommends at least 16 byte alignment for arrays
	u8 pad[16 - sizeof(usize) - sizeof(u32)];
	u8 data[];
};

void string_rc_decref(struct string_rc *str);

#define STRING_H
#endif
