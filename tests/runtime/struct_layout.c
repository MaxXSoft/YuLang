#include <assert.h>
#include <stddef.h>
#include <stdint.h>

struct Padded {
  uint8_t first;
  uint64_t wide;
  uint8_t last;
};
struct Nested {
  uint8_t first;
  struct Padded value;
  uint16_t last;
};
struct Arrays {
  uint8_t first;
  struct Padded values[2];
  uint8_t last;
};

extern size_t padded_size(void);
extern size_t nested_size(void);
extern size_t arrays_size(void);
extern size_t empty_size(void);
extern size_t padded_stride(void);
extern size_t nested_stride(void);
extern size_t arrays_stride(void);

int main(void) {
  assert(padded_size() == sizeof(struct Padded));
  assert(nested_size() == sizeof(struct Nested));
  assert(arrays_size() == sizeof(struct Arrays));
  assert(padded_stride() == padded_size());
  assert(nested_stride() == nested_size());
  assert(arrays_stride() == arrays_size());
  assert(empty_size() == 0);
  return 0;
}
