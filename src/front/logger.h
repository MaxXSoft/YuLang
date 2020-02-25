#ifndef YULANG_FRONT_LOGGER_H_
#define YULANG_FRONT_LOGGER_H_

#include <string_view>
#include <cstddef>

namespace yulang::front {

class Logger {
 public:
  Logger() : cur_file_(""), line_pos_(1), col_pos_(0) {}
  Logger(std::string_view cur_file)
      : cur_file_(cur_file), line_pos_(1), col_pos_(0) {}

  // print error message to stderr
  void LogError(std::string_view message) const;

  // reset error number
  void Reset() {
    line_pos_ = 1;
    col_pos_ = 0;
    error_num_ = 0;
  }
  // increase line position
  void IncreaseLinePos() {
    ++line_pos_;
    col_pos_ = 0;
  }
  // increase column position
  void IncreaseColPos() { ++col_pos_; }

  // getters
  std::string_view cur_file() const { return cur_file_; }
  std::size_t line_pos() const { return line_pos_; }
  std::size_t col_pos() const { return col_pos_; }
  std::size_t error_num() const { return error_num_; }

 private:
  static std::size_t error_num_;
  std::string_view cur_file_;
  std::size_t line_pos_, col_pos_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_LOGGER_H_
