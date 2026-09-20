#include <stdio.h>

extern int check_stack(void);

int main(void) {
  int result = check_stack();
  if (result) {
    fprintf(stderr, "Stack boundary, LIFO, or clear check failed: %d\n",
            result);
  }
  return result;
}
