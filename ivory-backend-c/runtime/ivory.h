#ifndef IVORY_H
#define IVORY_H

#include <stdint.h>
#include <math.h>
#include <string.h>
#include <limits.h>

#include "ivory_asserts.h"
#include "ivory_templates.h"

#define IFOREVER true
#define IFOREVER_INC

// ----------------------------------------

#define TYPE char
#define EXT  char
#define M    CHAR

#if (CHAR_MIN < 0)
ABS_I
SIGNUM_I
ADD_OVF_I
SUB_OVF_I
MUL_OVF_I
DIV_OVF_I
#else
static inline char abs_char(char i) {
  return i;
}
SIGNUM_U
ADD_OVF_U
SUB_OVF_U
MUL_OVF_U

#endif

#undef TYPE
#undef EXT
#undef M

// ----------------------------------------

#define TYPE int8_t
#define EXT  i8
#define M    INT8
ABS_I
SIGNUM_I
ADD_OVF_I
SUB_OVF_I
MUL_OVF_I
DIV_OVF_I
#undef TYPE
#undef EXT
#undef M

// ----------------------------------------


#define TYPE int16_t
#define EXT  i16
#define M    INT16
ABS_I
SIGNUM_I
ADD_OVF_I
SUB_OVF_I
MUL_OVF_I
DIV_OVF_I
#undef TYPE
#undef EXT
#undef M

// ----------------------------------------

#define TYPE int32_t
#define EXT  i32
#define M    INT32
ABS_I
SIGNUM_I
ADD_OVF_I
SUB_OVF_I
MUL_OVF_I
DIV_OVF_I
#undef TYPE
#undef EXT
#undef M

// ----------------------------------------

#define TYPE int64_t
#define EXT  i64
#define M    INT64
ABS_I
SIGNUM_I
ADD_OVF_I
SUB_OVF_I
MUL_OVF_I
DIV_OVF_I
#undef TYPE
#undef EXT
#undef M

// ----------------------------------------
// The types below don't need an abs implementation
// ----------------------------------------

// ----------------------------------------

#define TYPE float
#define EXT  float
SIGNUM_I
#undef TYPE
#undef EXT

// ----------------------------------------

#define TYPE double
#define EXT  double
SIGNUM_I
#undef TYPE
#undef EXT

// ----------------------------------------

#define TYPE uint8_t
#define EXT  u8
#define M    UINT8
SIGNUM_U
ADD_OVF_U
SUB_OVF_U
MUL_OVF_U
DIV_OVF_U
#undef TYPE
#undef EXT
#undef M

// ----------------------------------------

#define TYPE uint16_t
#define EXT  u16
#define M    UINT16
SIGNUM_U
ADD_OVF_U
SUB_OVF_U
MUL_OVF_U
DIV_OVF_U
#undef TYPE
#undef EXT
#undef M

// ----------------------------------------

#define TYPE uint32_t
#define EXT  u32
#define M    UINT32
SIGNUM_U
ADD_OVF_U
SUB_OVF_U
MUL_OVF_U
DIV_OVF_U
#undef TYPE
#undef EXT
#undef M

// ----------------------------------------

#define TYPE uint64_t
#define EXT  u64
#define M    UINT64
SIGNUM_U
ADD_OVF_U
SUB_OVF_U
MUL_OVF_U
DIV_OVF_U
#undef TYPE
#undef EXT
#undef M

// ----------------------------------------

// Index type: machine-depdentent size
typedef int idx;

#endif // IVORY_H
