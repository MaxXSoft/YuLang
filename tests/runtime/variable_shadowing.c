#include <stdint.h>
#include <stdio.h>

extern int32_t shadow_chain(int32_t), shadow_saved_pointer(int32_t),
    shadow_saved_reference(int32_t), shadow_with_reference(int32_t),
    shadow_parameter(int32_t), shadow_nested(int32_t),
    shadow_constant_to_runtime(int32_t), shadow_runtime_to_constant(int32_t),
    shadow_definition_list(int32_t), shadow_function(int32_t),
    shadow_saved_function(int32_t);
extern uint64_t shadow_type(int32_t);
extern uintptr_t shadow_array_length(void);
extern int32_t shadow_inline_constant(void);
extern int32_t shadow_mutability(int32_t), shadow_var_parameter(int32_t),
    shadow_uninitialized_var(int32_t), shadow_saved_mutable_storage(int32_t),
    shadow_var_definition_list(int32_t), shadow_var_constant_mask(int32_t),
    shadow_var_nested(int32_t);
extern uint64_t shadow_var_type(int32_t);

static int failures;
static void check(int condition, const char *expression) {
  if (!condition) {
    fprintf(stderr, "failed: %s\n", expression);
    ++failures;
  }
}
#define CHECK(expr) check((expr), #expr)

int main(void) {
  CHECK(shadow_chain(5) == 12);
  CHECK(shadow_type(5) == UINT64_C(4294967301));
  CHECK(shadow_saved_pointer(5) == 506);
  CHECK(shadow_saved_reference(5) == 1506);
  CHECK(shadow_with_reference(5) == 6);
  CHECK(shadow_parameter(5) == 12);
  CHECK(shadow_nested(5) == 116006);
  CHECK(shadow_constant_to_runtime(5) == 6);
  CHECK(shadow_runtime_to_constant(5) == 10);
  CHECK(shadow_inline_constant() == 9);
  CHECK(shadow_array_length() == 812);
  CHECK(shadow_definition_list(5) == 12);
  CHECK(shadow_function(5) == 25);
  CHECK(shadow_saved_function(5) == 1525);
  CHECK(shadow_mutability(5) == 36);
  CHECK(shadow_var_parameter(5) == 12);
  CHECK(shadow_var_type(5) == UINT64_C(4294967301));
  CHECK(shadow_uninitialized_var(5) == 509);
  CHECK(shadow_saved_mutable_storage(5) == 1508);
  CHECK(shadow_var_definition_list(5) == 13);
  CHECK(shadow_var_constant_mask(5) == 6);
  CHECK(shadow_var_nested(5) == 117006);
  return failures ? 1 : 0;
}
