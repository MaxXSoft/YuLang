#include <stdbool.h>
#include <stdio.h>

extern bool inferred_initializers(bool);

int main(void) {
  if (!inferred_initializers(false) || !inferred_initializers(true)) {
    fputs("inferred aggregate values or evaluation order differ\n", stderr);
    return 1;
  }
  return 0;
}
