#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

extern void output_text(void);
extern void output_integers(void);
extern void output_empty(void);

static char output[256];
static size_t output_size;
static size_t max_chunk;
static size_t calls;
static size_t fail_after;
static int failure;

// Export the write symbol consumed by the separately compiled Yu module.
// NOLINTNEXTLINE(misc-use-internal-linkage)
ssize_t write(int fd, const void *data, size_t size) {
  ++calls;
  if (fd != 1 || output_size + size > sizeof(output)) {
    fputs("Unexpected write arguments\n", stderr);
    return -1;
  }
  if (fail_after && calls >= fail_after) {
    errno = EIO;
    return failure;
  }
  if (max_chunk && size > max_chunk) {
    size = max_chunk;
  }
  memcpy(output + output_size, data, size);
  output_size += size;
  return (ssize_t)size;
}

static void reset(size_t chunk, size_t stop, int result) {
  output_size = 0;
  calls = 0;
  max_chunk = chunk;
  fail_after = stop;
  failure = result;
}

static int matches(const char *expected) {
  return output_size == strlen(expected) &&
         memcmp(output, expected, output_size) == 0;
}

int main(void) {
  const char *text =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  reset(0, 0, 0);
  output_integers();
  if (!matches("-2147483648 0 2147483647")) {
    fputs("Signed integer output is incorrect\n", stderr);
    return 1;
  }
  reset(0, 0, 0);
  output_text();
  if (!matches(text) || calls != 1) {
    fputs("String output is not batched into one write\n", stderr);
    return 1;
  }
  reset(3, 0, 0);
  output_text();
  if (!matches(text) || calls != (strlen(text) + 2) / 3) {
    fputs("Short string writes lost bytes\n", stderr);
    return 1;
  }
  reset(3, 0, 0);
  output_integers();
  if (!matches("-2147483648 0 2147483647")) {
    fputs("Short integer writes lost bytes\n", stderr);
    return 1;
  }
  reset(3, 3, -1);
  output_text();
  if (calls != 3 || output_size != 6 || memcmp(output, text, 6) != 0) {
    fputs("Failed writes did not stop\n", stderr);
    return 1;
  }
  reset(3, 3, 0);
  output_text();
  if (calls != 3 || output_size != 6) {
    fputs("Zero-byte writes did not stop\n", stderr);
    return 1;
  }
  reset(0, 0, 0);
  output_empty();
  if (calls || output_size) {
    fputs("Empty string issued a write\n", stderr);
    return 1;
  }
  return 0;
}
