#ifndef BINARY_H
#define BINARY_H

#include "term.h"

#define BIN_BYTE_SIZE(bit_size)			(((bit_size) + 7) >> 3)
#define BIN_WHOLE_BYTES(bit_size)		((bit_size) >> 3)
#define BIN_TRAILER_SIZE(bit_size)		((bit_size) & 7)
#define BIN_MASK(bit_size)				((1 << (bit_size)) - 1)

#endif
