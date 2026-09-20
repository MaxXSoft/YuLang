#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

extern bool float_lt(void), float_le(void), float_gt(void), float_ge(void);
extern bool float_false(void), float32_lt(void);
extern bool unsigned_lt(void), unsigned_le(void), unsigned_gt(void),
    unsigned_ge(void);
extern double signed_float64(void), unsigned_float64(void);
extern float signed_float32(void);
extern float repeated_float32(void), local_float32(void),
    arithmetic_float32(void);
extern int32_t narrow_signed_div(void), narrow_signed_mod(void),
    narrow_signed_shift(void);
extern bool narrow_signed_less(void), unsigned8_wrap(void);
extern int64_t signed8_widen(void), signed16_widen(void), signed32_widen(void),
    signed64_mul(void);
extern uint64_t unsigned16_wrap(void), unsigned32_wrap(void),
    unsigned8_sub(void);
extern uint64_t unsigned8_mul(void), unsigned8_shl(void), unsigned8_not(void),
    unsigned8_neg(void);
extern int64_t float_signed_int(void);
extern uint64_t float_unsigned_int(void);
extern bool runtime_float_lt(double, double),
    runtime_unsigned_lt(uint64_t, uint64_t);
extern double runtime_signed_float(int32_t);
extern int32_t runtime_signed_div(int32_t);
extern uint64_t runtime_unsigned_wrap(uint32_t);

static int failures;
static void check(int condition, const char *expression) {
  if (!condition) {
    fprintf(stderr, "failed: %s\n", expression);
    ++failures;
  }
}
#define CHECK(expr) check((expr), #expr)

int main(void) {
  CHECK(float_lt());
  CHECK(float_le());
  CHECK(float_gt());
  CHECK(float_ge());
  CHECK(!float_false());
  CHECK(float32_lt());
  CHECK(!unsigned_lt());
  CHECK(!unsigned_le());
  CHECK(unsigned_gt());
  CHECK(unsigned_ge());
  CHECK(signed_float64() == -1.0);
  CHECK(signed_float32() == -7.0F);
  CHECK(unsigned_float64() == 0x1p63);
  CHECK(narrow_signed_div() == 0);
  CHECK(narrow_signed_mod() == -1);
  CHECK(narrow_signed_shift() == -2);
  CHECK(narrow_signed_less());
  CHECK(signed8_widen() == -1);
  CHECK(signed16_widen() == -1);
  CHECK(signed32_widen() == -1);
  CHECK(unsigned8_wrap());
  CHECK(unsigned16_wrap() == 0);
  CHECK(unsigned32_wrap() == 0);
  CHECK(unsigned8_sub() == 255);
  CHECK(unsigned8_mul() == 0);
  CHECK(unsigned8_shl() == 0);
  CHECK(unsigned8_not() == 255);
  CHECK(unsigned8_neg() == 255);
  CHECK(signed64_mul() == -2);
  CHECK(float_signed_int() == -7);
  CHECK(float_unsigned_int() == 255);
  CHECK(float_lt() == runtime_float_lt(1.0, 2.0));
  CHECK(unsigned_lt() == runtime_unsigned_lt(UINT64_MAX, 0));
  CHECK(signed_float64() == runtime_signed_float(-1));
  CHECK(narrow_signed_div() == runtime_signed_div(-1));
  CHECK(unsigned32_wrap() == runtime_unsigned_wrap(UINT32_MAX));
  CHECK(repeated_float32() == 1.5F);
  CHECK(local_float32() == 2.5F);
  CHECK(arithmetic_float32() == 3.5F);
  return failures ? 1 : 0;
}
