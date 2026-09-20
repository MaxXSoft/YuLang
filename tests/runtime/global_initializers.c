#include <assert.h>
#include <stdbool.h>

static int trace;

int tick(int value) {
  trace = trace * 10 + value;
  return value;
}

extern bool check_initializers(void);

int main(void) {
  assert(trace == 12345678);
  assert(check_initializers());
  return 0;
}
