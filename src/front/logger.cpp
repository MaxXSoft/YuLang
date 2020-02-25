#include "front/logger.h"

#include <iostream>

#include "xstl/style.h"

using namespace yulang::front;

// definition of static member variables in logger
std::size_t Logger::error_num_;

// TODO: show content of file

void Logger::LogError(std::string_view message) const {
  using namespace xstl;
  // print error message
  std::cerr << style("B") << cur_file_ << ":";
  std::cerr << style("B") << line_pos_ << ":" << col_pos_ << ":";
  std::cerr << style("Br") << "error: ";
  std::cerr << message << std::endl;
  // increase error number
  ++error_num_;
}
