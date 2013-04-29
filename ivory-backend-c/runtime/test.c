#include "ivory.h"
#include "inttypes.h"
#include "stdio.h"

int main(void) {
  int i;
  scanf("%d", &i);
  printf("%"PRIi8, abs_i8((int8_t) i));

  return 0;
}
