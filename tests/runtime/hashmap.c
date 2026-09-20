#include <stdio.h>

extern int check_hashmap(void);

int main(void) {
  int result = check_hashmap();
  if (result) {
    fprintf(stderr, "HashMap growth or content check failed: %d\n", result);
  }
  return result;
}
