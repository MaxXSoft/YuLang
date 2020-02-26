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
  // print warning message to stderr
  void LogWarning(std::string_view message) const;

  // reset error number
  void Reset() {
    line_pos_ = 1;
    col_pos_ = 0;
    error_num_ = 0;
    warning_num_ = 0;
  }
  // increase line position
  void IncreaseLinePos() {
    ++line_pos_;
    col_pos_ = 0;
  }
  // increase column position
  void IncreaseColPos() { ++col_pos_; }

  // setters
  void set_warn_as_err(bool warn_as_err) { warn_as_err_ = warn_as_err; }

  // getters
  std::string_view cur_file() const { return cur_file_; }
  std::size_t line_pos() const { return line_pos_; }
  std::size_t col_pos() const { return col_pos_; }
  std::size_t error_num() const { return error_num_; }
  std::size_t warning_num() const { return warning_num_; }

 private:
  void LogFileInfo() const;

  static std::size_t error_num_, warning_num_;
  static bool warn_as_err_;
  std::string_view cur_file_;
  std::size_t line_pos_, col_pos_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_LOGGER_H_
