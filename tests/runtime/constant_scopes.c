#include <stdint.h>
#include <stdio.h>
extern int32_t shadow_parameter(int32_t), shadow_parameter_init(int32_t);
extern int32_t shadow_var(void), shadow_uninitialized(void),
    shadow_nonconstant(void);
extern int32_t shadow_reference(void), shadow_initializer(void),
    nested_constant(void);
extern int32_t nested_then_outer(void), outer_constant(void), shadow_loop(void);
extern uintptr_t local_array_size(void), outer_array_size(void);
extern int32_t scope_input(void);
int32_t scope_input(void) { return 9; }
static int failures;
static void check(int condition, const char *expression) {
  if (!condition) {
    fprintf(stderr, "failed: %s\n", expression);
    ++failures;
  }
}
#define CHECK(expr) check((expr), #expr)

int main(void) {
  CHECK(shadow_parameter(42) == 42);
  CHECK(shadow_parameter_init(42) == 43);
  CHECK(shadow_var() == 9);
  CHECK(shadow_uninitialized() == 9);
  CHECK(shadow_nonconstant() == 9);
  CHECK(shadow_reference() == 9);
  CHECK(shadow_initializer() == 8);
  CHECK(nested_constant() == 11);
  CHECK(nested_then_outer() == 7);
  CHECK(outer_constant() == 7);
  CHECK(local_array_size() == 3 * sizeof(int32_t));
  CHECK(outer_array_size() == 2 * sizeof(int32_t));
  CHECK(shadow_loop() == 3);
  return failures ? 1 : 0;
}
