#include <assert.h>
#include <stdint.h>

struct Register {
  volatile uint32_t status;
};
extern void read_discard(const volatile uint32_t *);
extern void read_twice(const volatile uint32_t *);
extern void read_unused_sum(const volatile uint32_t *);
extern uint32_t read_value(const volatile uint32_t *);
extern void write_value(volatile uint32_t *, uint32_t);
extern void add_value(volatile uint32_t *, uint32_t);
extern const volatile uint32_t *address_of(const volatile uint32_t *);
extern void pass_reference(const volatile uint32_t *);
extern void read_field(const struct Register *);
extern void write_field(struct Register *, uint32_t);
extern void read_indirect(const volatile uint32_t *volatile *);

static const volatile uint32_t *reference_address;
// Called by the separately compiled Yu fixture through its C ABI.
// NOLINTNEXTLINE(misc-use-internal-linkage)
void reference_sink(const volatile uint32_t *value) {
  reference_address = value;
}

int main(void) {
  volatile uint32_t value = 5;
  read_discard(&value);
  read_twice(&value);
  read_unused_sum(&value);
  assert(read_value(&value) == 5);
  write_value(&value, 7);
  assert(value == 7);
  add_value(&value, 3);
  assert(value == 10);
  assert(address_of(&value) == &value);
  pass_reference(&value);
  assert(reference_address == &value);
  struct Register reg = {11};
  read_field(&reg);
  write_field(&reg, 12);
  assert(reg.status == 12);
  const volatile uint32_t *volatile pointer = &value;
  read_indirect(&pointer);
  return 0;
}
