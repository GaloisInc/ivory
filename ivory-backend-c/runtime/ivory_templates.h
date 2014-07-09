#ifndef IVORY_TEMPLATES_H
#define IVORY_TEMPLATES_H

#include<stdbool.h>
#include<limits.h>

#define CAT(X,Y) X##_##Y
// Indirection in case X,Y defined
#define C(X,Y) CAT(X,Y)

#define MKMAX(X) C(X,MAX)
#define MKMIN(X) C(X,MIN)

// abs implementations for signed types
// note: -MIN may not equal MAX
#define ABS_I                                       \
static inline TYPE C(abs,EXT)(TYPE i) {             \
  if(i == MKMIN(M)) { return MKMAX(M); }            \
  else { return i >= 0 ? i : -i; }                  \
}

// Unsigned signnum
#define SIGNUM_U                                    \
static inline TYPE C(signum,EXT)(TYPE i) {          \
  if (i > 0) return 1;                              \
  return 0;                                         \
}

// Signed signum
#define SIGNUM_I                                    \
static inline TYPE C(signum,EXT)(TYPE i) {          \
  if (i > 0) return 1;                              \
  if (i < 0) return (-1);                           \
  return 0;                                         \
}

// Addition overflow check for signed types
#define ADD_OVF_I                                   \
static inline bool C(add_ovf,EXT)(TYPE x, TYPE y) { \
  return (x >= 0 && y >= 0 && MKMAX(M) - x >= y)    \
      || (x <= 0 && y <= 0 && MKMIN(M) - x <= y)    \
      || (C(signum,EXT)(x) != C(signum,EXT)(y));    \
}

// Addition overflow check for unsigned types
#define ADD_OVF_U                                   \
static inline bool C(add_ovf,EXT)(TYPE x, TYPE y) { \
  return MKMAX(M) - x >= y;                         \
}

// Subtraction overflow check for signed types
#define SUB_OVF_I                                   \
static inline bool C(sub_ovf,EXT)(TYPE x, TYPE y) { \
  return (x >= 0 && y <= 0 && MKMAX(M) + y >= x)    \
      || (x <= 0 && y >= 0 && MKMIN(M) + y <= x)    \
      || (C(signum,EXT)(x) == C(signum,EXT)(y));    \
}

// Subtraction overflow check for unsigned types
#define SUB_OVF_U                                   \
static inline bool C(sub_ovf,EXT)(TYPE x, TYPE y) { \
  return x >= y;                                    \
}

// Multiplication overflow check for signed types.  Note use of our safe abs
// function here.
#define MUL_OVF_I                                          \
static inline bool C(mul_ovf,EXT)(TYPE x, TYPE y) {        \
  return (x != MKMIN(M) || y != (-1))                      \
      && (y != MKMIN(M) || x != (-1))                      \
      && (   x == 0                                        \
          || y == 0                                        \
          || (   MKMAX(M) / C(abs,EXT)(x) >= C(abs,EXT)(y) \
              && MKMIN(M) / C(abs,EXT)(x) <= C(abs,EXT)(y) \
         ));                                               \
}

// Multiplication overflow check for unsigned types.
#define MUL_OVF_U                                    \
static inline bool C(mul_ovf,EXT)(TYPE x, TYPE y) {  \
  return x == 0 || MKMAX(M) / x >= y;                \
}

// Division overflow check for signed types.
#define DIV_OVF_I                                    \
static inline bool C(div_ovf,EXT)(TYPE x, TYPE y) {  \
  return y != 0 && (x != MKMIN(M) || y != (-1));     \
}

// Division overflow check for unsigned types.
#define DIV_OVF_U                                                             \
  static inline bool C(div_ovf,EXT)(__attribute__((unused)) TYPE x, TYPE y) { \
  return y != 0;                                                              \
}

#endif // IVORY_TEMPLATES_H
