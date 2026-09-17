#include "front/logger.h"

#include <iostream>

#include "xstl/style.h"

namespace yulang::front {

// definition of static member variables in logger
std::size_t Logger::error_num_, Logger::warning_num_;
bool Logger::warn_as_err_;

// TODO(YuLang): show content of file

void Logger::LogFileInfo() const {
  using xstl::style;
  std::cerr << style("B") << cur_file_ << ":";
  std::cerr << style("B") << line_pos_ << ":" << col_pos_ << ": ";
}

void Logger::LogRawError(std::string_view message) {
  using xstl::style;
  // print error message
  std::cerr << style("Br") << "error: ";
  std::cerr << message << '\n';
  // increase error number
  ++error_num_;
}

void Logger::LogError(std::string_view message) const {
  LogFileInfo();
  LogRawError(message);
}

void Logger::LogError(std::string_view message, std::string_view id) const {
  using xstl::style;
  LogFileInfo();
  // print error message
  std::cerr << style("Br") << "error: ";
  std::cerr << "id: " << id << ", " << message << '\n';
  // increase error number
  ++error_num_;
}

// print warning message to stderr
void Logger::LogWarning(std::string_view message) const {
  using xstl::style;
  // log all warnings as errors
  if (warn_as_err_) {
    LogError(message);
    return;
  }
  // print warning message
  LogFileInfo();
  std::cerr << style("Bp") << "warning: ";
  std::cerr << message << '\n';
  // increase warning number
  ++warning_num_;
}

}  // namespace yulang::front
