#ifndef IVORY_H
#define IVORY_H

#include<stdint.h>
#include<math.h>
#include<stdbool.h>
#include<string.h>
#include<limits.h>

#include "ivory_asserts.h"

#define FOREVER true
#define FOREVER_INC

/* abs implementations */

#if (CHAR_MIN < 0)
static inline char abs_char(char i) {
  COMPILER_ASSERTS(i != CHAR_MIN);
  return i >= 0 ? i : -i;
}
#else
static inline char abs_char(char i) {
  return i;
}
#endif

static inline int8_t abs_i8(int8_t i) {
  COMPILER_ASSERTS(i != INT8_MIN);
  return i >= 0 ? i : -i;
}

static inline int16_t abs_i16(int16_t i) {
  COMPILER_ASSERTS(i != INT16_MIN);
  return i >= 0 ? i : -i;
}

static inline int32_t abs_i32(int32_t i) {
  COMPILER_ASSERTS(i != INT32_MIN);
  return i >= 0 ? i : -i;
}

static inline int64_t abs_i64(int64_t i) {
  COMPILER_ASSERTS(i != INT64_MIN);
  return i >= 0 ? i : -i;
}

/* signum implementations */

#if (CHAR_MIN < 0)
static inline char signum_char(char i) {
  if (i > 0) return 1;
  if (i < 0) return (-1);
  return 0;
}
#else
static inline char signum_char(char i) {
  if (i > 0) return 1;
  return 0;
}
#endif

static inline int8_t signum_i8(int8_t i) {
  if (i > 0) return 1;
  if (i < 0) return (-1);
  return 0;
}

static inline int16_t signum_i16(int16_t i) {
  if (i > 0) return 1;
  if (i < 0) return (-1);
  return 0;
}

static inline int32_t signum_i32(int32_t i) {
  if (i > 0) return 1;
  if (i < 0) return (-1);
  return 0;
}

static inline int64_t signum_i64(int64_t i) {
  if (i > 0) return 1;
  if (i < 0) return (-1);
  return 0;
}

static inline float signum_float(float i) {
  if (i > 0) return 1;
  if (i < 0) return (-1);
  return 0;
}

static inline double signum_double(double i) {
  if (i > 0) return 1;
  if (i < 0) return (-1);
  return 0;
}

static inline uint8_t signum_u8(uint8_t i) {
  if (i > 0) return 1;
  return 0;
}

static inline uint16_t signum_u16(uint16_t i) {
  if (i > 0) return 1;
  return 0;
}

static inline uint32_t signum_u32(uint32_t i) {
  if (i > 0) return 1;
  return 0;
}

static inline uint64_t signum_u64(uint64_t i) {
  if (i > 0) return 1;
  return 0;
}

/* index type */

/* machine-depdentent size */
typedef int idx;

#endif // IVORY_H
