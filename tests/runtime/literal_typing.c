#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

extern uint64_t literal_arithmetic(uint64_t), literal_maximum(void),
    literal_mask(void);
extern int64_t literal_minimum(void);
extern int64_t literal_narrow(void);
extern uint32_t literal_branches(bool, uint32_t);
extern const uint8_t *literal_null(bool, const uint8_t *);
extern bool literal_checks(void);

static int failures;
static void check(bool condition, const char *expression) {
  if (!condition) {
    fprintf(stderr, "failed: %s\n", expression);
    ++failures;
  }
}
#define CHECK(expr) check((expr), #expr)

int main(void) {
  const uint8_t byte = 42;
  CHECK(literal_arithmetic(5) == (UINT64_C(2) << 40U) + 18);
  CHECK(literal_minimum() == INT64_MIN);
  CHECK(literal_narrow() == INT8_MIN);
  CHECK(literal_maximum() == UINT64_MAX);
  CHECK(literal_mask() == UINT64_MAX);
  CHECK(literal_branches(true, 9) == 100);
  CHECK(literal_branches(false, 9) == 200);
  CHECK(literal_null(false, &byte) == NULL);
  CHECK(literal_null(true, &byte) == &byte);
  CHECK(literal_checks());
  return failures ? 1 : 0;
}
