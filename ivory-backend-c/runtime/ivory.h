#ifndef IVORY_H
#define IVORY_H

#include<stdint.h>
#include<math.h>
#include<stdbool.h>
#include<string.h>

/* Requires and Provides statements */

#ifdef IVORY_TEST

#include <assert.h>

#define REQUIRES(arg) assert(arg)
#define ENSURES(arg)  assert(arg)
#define ASSUMES(arg)  assert(arg)
#define ASSERTS(arg)  assert(arg)

#endif /* IVORY_TEST */

#ifdef IVORY_CBMC

#define REQUIRES(arg) __CPROVER_assume(arg)
#define ENSURES(arg)  __CPROVER_assert(arg, "")
#define ASSERTS(arg)  __CPROVER_assert(arg, "")
#define ASSUMES(arg)  __CPROVER_assume(arg)

#endif /* IVORY_CBMC */

#ifdef IVORY_DEPLOY

#define REQUIRES(arg)
#define ENSURES(arg)
#define ASSERTS(arg)
#define ASSUMES(arg)

#endif /* IVORY_DEPLOY */

/* So CBMC can handle forever loops. */
#ifdef IVORY_CBMC
#define FOREVER forever_loop < 1
#define FOREVER_INC forever_loop++
#else
#define FOREVER true
#define FOREVER_INC
#endif

/* abs implementations */

static inline char abs_char(char i) {
  return i >= 0 ? i : -i;
}

static inline int8_t abs_i8(int8_t i) {
  return i >= 0 ? i : -i;
}

static inline int16_t abs_i16(int16_t i) {
  return i >= 0 ? i : -i;
}

static inline int32_t abs_i32(int32_t i) {
  return i >= 0 ? i : -i;
}

static inline int64_t abs_i64(int64_t i) {
  return i >= 0 ? i : -i;
}

/* signum implementations */

static inline char signum_char(char i) {
  if (i > 0) return 1;
  if (i < 0) return (-1);
  return 0;
}

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

/* dynamic arrays */

struct ivory_dynarray {
  void *data;
  idx length;
};

#endif // IVORY_H
