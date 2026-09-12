#include <stdbool.h>

int check_zeros(const void *pointer, int integer, float single,
                double real, bool boolean) {
  return pointer != 0 || integer != 0 || single != 0.0f ||
         real != 0.0 || boolean;
}
