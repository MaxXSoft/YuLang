#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

extern int8_t bool_to_i8(bool);
extern uint8_t bool_to_u8(bool);
extern int32_t bool_to_i32(bool);
extern uint64_t bool_to_u64(bool);
extern float bool_to_f32(bool);
extern double bool_to_f64(bool);
extern bool i8_to_bool(int8_t);
extern bool u8_to_bool(uint8_t);
extern bool i32_to_bool(int32_t);
extern bool f32_to_bool(float);
extern bool f64_to_bool(double);
extern bool pointer_to_bool(const uint8_t *);
extern int32_t literal_true(void);
extern bool literal_two(void);

int main(void) {
  assert(bool_to_i32(true) == literal_true());
  for (int value = 0; value <= 1; ++value) {
    assert(bool_to_i8(value) == value);
    assert(bool_to_u8(value) == value);
    assert(bool_to_i32(value) == value);
    assert(bool_to_u64(value) == (uint64_t)value);
    assert(bool_to_f32(value) == value);
    assert(bool_to_f64(value) == value);
  }
  for (int value = -128; value <= 127; ++value) {
    assert(i8_to_bool(value) == (value != 0));
  }
  for (unsigned value = 0; value <= 255; ++value) {
    assert(u8_to_bool(value) == (value != 0));
  }
  assert(u8_to_bool(2) == literal_two());
  assert(!i32_to_bool(0));
  assert(i32_to_bool(-42));
  const double floating[] = {0.0, -0.0, 0.5, -0.5, 2.0, INFINITY, NAN};
  for (unsigned i = 0; i < sizeof(floating) / sizeof(floating[0]); ++i) {
    assert(f32_to_bool(floating[i]) == (floating[i] != 0.0));
    assert(f64_to_bool(floating[i]) == (floating[i] != 0.0));
  }
  uint8_t bytes[2];
  assert(!pointer_to_bool(0));
  assert(pointer_to_bool(bytes));
  assert(pointer_to_bool(bytes + 1));
  return 0;
}
