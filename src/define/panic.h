#ifndef YULANG_DEFINE_PANIC_H_
#define YULANG_DEFINE_PANIC_H_

#include <cstdlib>
#include <iostream>

#include "xstl/style.h"

// Prints error message and abort.
#define PANIC(msg)                                                       \
  do {                                                                   \
    std::cerr << xstl::style("Br") << "error: ";                         \
    std::cerr << "compilation failed with an internal compiler error: "; \
    std::cerr << (msg) << '\n';                                          \
    std::abort();                                                        \
  } while (0)

#endif  // YULANG_DEFINE_PANIC_H_
