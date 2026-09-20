#include <stdint.h>
#include <stdio.h>
extern int32_t when_selected(void), when_runtime(int32_t),
    when_unknown(int32_t);
extern int32_t when_unknown_before_match(void), when_unknown_in_list(void);
extern int32_t when_match_before_unknown(void), when_first_match(void);
extern int32_t when_unselected_effect(void), when_nested(void);
extern void when_empty_match(void);
static int effects, conditions, condition_value, failures;
int32_t when_effect(void) {
  ++effects;
  return 8;
}
int32_t when_condition(void) {
  ++conditions;
  return condition_value;
}
#define CHECK(expr)                           \
  do {                                        \
    if (!(expr)) {                            \
      fprintf(stderr, "failed: %s\n", #expr); \
      ++failures;                             \
    }                                         \
  } while (0)
int main(void) {
  CHECK(when_selected() == 8);
  CHECK(effects == 1);
  CHECK(when_runtime(1) == 8);
  CHECK(effects == 2);
  CHECK(when_runtime(0) == 2);
  CHECK(effects == 2);
  CHECK(when_unknown(1) == 3);
  CHECK(when_unknown(0) == 2);
  condition_value = 1;
  CHECK(when_unknown_before_match() == 3);
  CHECK(conditions == 1);
  condition_value = 0;
  CHECK(when_unknown_before_match() == 4);
  CHECK(conditions == 2);
  CHECK(when_unknown_in_list() == 3);
  CHECK(conditions == 3);
  CHECK(when_match_before_unknown() == 3);
  CHECK(conditions == 3);
  CHECK(when_first_match() == 3);
  CHECK(when_unselected_effect() == 2);
  CHECK(effects == 2);
  CHECK(when_nested() == 5);
  when_empty_match();
  CHECK(effects == 3);
  return failures ? 1 : 0;
}
