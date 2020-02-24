#ifndef YULANG_FRONT_LOGGER_H_
#define YULANG_FRONT_LOGGER_H_

#include <string_view>
#include <cstddef>

namespace yulang::front {

class Logger {
 public:
  Logger() {}
  Logger(std::string_view cur_file) : cur_file_(cur_file), line_pos_(0) {}

  // print error message to stderr
  void LogError(std::string_view message);

  // setters
  void set_cur_file(std::string_view cur_file) {
    cur_file_ = cur_file;
  }
  void set_line_num(std::size_t line_pos) { line_pos_ = line_pos; }

 private:
  std::string_view cur_file_;
  std::size_t line_pos_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_LOGGER_H_
