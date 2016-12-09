#ifndef IVORY_ASSERTS_H
#define IVORY_ASSERTS_H

/* Requires and Provides statements */

#ifdef IVORY_TEST

#if defined(IVORY_USER_ASSERT_HOOK)

extern void ivory_user_assert_hook(void);

#define ivory_assert(arg)         \
  do {                            \
    if (!(arg)) {                 \
       ivory_user_assert_hook();  \
    }                             \
  } while (0)

#define REQUIRES(arg)         ivory_assert(arg)
#define ENSURES(arg)          ivory_assert(arg)
#define ASSUMES(arg)          ivory_assert(arg)
#define ASSERTS(arg)          ivory_assert(arg)
#define COMPILER_ASSERTS(arg) ivory_assert(arg)

#elif defined(IVORY_USER_VERBOSE_ASSERT_HOOK)

extern void ivory_user_verbose_assert_hook(const char *asserttype,
		const char *expr, const char *file, int line);

#define ivory_assert(atype, arg)  \
  do {                            \
    if (!(arg)) {                 \
       ivory_user_verbose_assert_hook(atype, #arg, __FILE__, __LINE__); \
    }                             \
  } while (0)

#define REQUIRES(arg)         ivory_assert("REQUIRES",arg)
#define ENSURES(arg)          ivory_assert("ENSURES",arg)
#define ASSUMES(arg)          ivory_assert("ASSUMES",arg)
#define ASSERTS(arg)          ivory_assert("ASSERTS",arg)
#define COMPILER_ASSERTS(arg) ivory_assert("COMPILER_ASSERTS",arg)

#else

#include <assert.h>

#define REQUIRES(arg)         assert(arg)
#define ENSURES(arg)          assert(arg)
#define ASSUMES(arg)          assert(arg)
#define ASSERTS(arg)          assert(arg)
#define COMPILER_ASSERTS(arg) assert(arg)

#endif /* assert hooks */

#else /* IVORY_TEST */

#define REQUIRES(arg)           (void)(arg)
#define ENSURES(arg)            (void)(arg)
#define ASSUMES(arg)            (void)(arg)
#define ASSERTS(arg)            (void)(arg)
#define COMPILER_ASSERTS(arg)   (void)(arg)

#endif /* IVORY_TEST */

#endif /* IVORY_ASSERTS_H */
