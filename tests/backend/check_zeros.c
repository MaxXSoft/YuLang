#include <stdbool.h>

// Called by the separately compiled Yu fixture through its C ABI.
// NOLINTNEXTLINE(misc-use-internal-linkage)
int check_zeros(const void *pointer, int integer, float single, double real,
                bool boolean) {
  return pointer != 0 || integer != 0 || single != 0.0F || real != 0.0 ||
         boolean;
}
