#include "front/logger.h"

#include <iostream>

#include "xstl/style.h"

using namespace yulang::front;

// TODO: show content of file

void Logger::LogError(std::string_view message) const {
  using namespace xstl;
  // print error message
  std::cerr << style("B") << cur_file_ << ":" << line_pos_ << ": ";
  std::cerr << style("RBr") << "error: ";
  std::cerr << message << std::endl;
}
