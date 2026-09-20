#include <stdio.h>

extern int check_dynarray_copy(void);
extern int check_dynarray_growth(void);
extern int check_dynarray_zero_capacity(void);

int main(void) {
  if (check_dynarray_copy()) {
    fputs("DynArray copy or self-assignment lost data\n", stderr);
    return 1;
  }
  if (check_dynarray_growth()) {
    fputs("DynArray resize failed to preserve and initialize elements\n",
          stderr);
    return 1;
  }
  if (check_dynarray_zero_capacity()) {
    fputs("DynArray failed to grow from zero capacity\n", stderr);
    return 1;
  }
  return 0;
}
