#ifndef IVORY_ASSERTS_H
#define IVORY_ASSERTS_H

/* Requires and Provides statements */

#ifdef IVORY_TEST

#ifdef __arm__

/* TODO: We could write a better "assert" that suspends all RTOS
 * tasks and tries to write a debug string somewhere, but this is at
 * least somewhat useful while running under GDB. */
#define ivory_assert(arg)         \
  do {                            \
    if (!(arg)) {                 \
      asm volatile("bkpt");       \
    }                             \
  } while (0)

#define REQUIRES(arg)         ivory_assert(arg)
#define ENSURES(arg)          ivory_assert(arg)
#define ASSUMES(arg)          ivory_assert(arg)
#define ASSERTS(arg)          ivory_assert(arg)
#define COMPILER_ASSERTS(arg) ivory_assert(arg)

#else /* ! __arm__ */

#include <assert.h>

#define REQUIRES(arg)         assert(arg)
#define ENSURES(arg)          assert(arg)
#define ASSUMES(arg)          assert(arg)
#define ASSERTS(arg)          assert(arg)
#define COMPILER_ASSERTS(arg) assert(arg)

#endif /* __arm__ */

#endif /* IVORY_TEST */

#ifdef IVORY_DEPLOY

#define REQUIRES(arg)
#define ENSURES(arg)
#define ASSUMES(arg)
#define ASSERTS(arg)
#define COMPILER_ASSERTS(arg)

#endif /* IVORY_DEPLOY */

#endif /* IVORY_ASSERTS_H */
