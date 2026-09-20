#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

// These Unix bindings require the pointer-sized long/time_t data model.
_Static_assert(sizeof(long) == sizeof(intptr_t), "unsupported C long ABI");
_Static_assert(sizeof(time_t) == sizeof(intptr_t), "unsupported time_t ABI");

extern intptr_t store_time(intptr_t *destination);
extern intptr_t parse_long(const char *text);
extern uintptr_t parse_ulong(const char *text);
extern intptr_t round_long(double value);
extern intptr_t round_float_long(float value);

int main(void) {
  struct {
    uintptr_t before;
    intptr_t value;
    uintptr_t after;
  } clock = {123456789, 0, 987654321};
  const time_t start = time(NULL);
  const intptr_t result = store_time(&clock.value);
  const time_t end = time(NULL);
  assert(clock.before == 123456789 && clock.after == 987654321);
  assert(result == clock.value && result >= start && result <= end);

  char text[32];
  snprintf(text, sizeof(text), "%ld", LONG_MIN);
  assert(parse_long(text) == LONG_MIN);
  snprintf(text, sizeof(text), "%ld", LONG_MAX);
  assert(parse_long(text) == LONG_MAX);
  snprintf(text, sizeof(text), "%lu", ULONG_MAX);
  assert(parse_ulong(text) == ULONG_MAX);
  assert(round_long(-123.5) == -124);
  assert(round_float_long(123.5F) == 124);
  if (sizeof(long) > 4) {
    assert(parse_long("4294967297") == INT64_C(4294967297));
    assert(parse_ulong("4294967297") == UINT64_C(4294967297));
    assert(round_long(4294967297.0) == INT64_C(4294967297));
    assert(round_float_long(4294967296.0F) == INT64_C(4294967296));
  }
  return 0;
}
