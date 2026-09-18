#ifndef YULANG_FRONT_LEXER_H_
#define YULANG_FRONT_LEXER_H_

#include <cassert>
#include <cstdint>
#include <fstream>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

#include "define/token.h"
#include "front/logger.h"

namespace yulang::front {

using MacroDefinitions = std::unordered_map<std::string, std::string>;

class Lexer {
 public:
  Lexer(std::string_view file, const MacroDefinitions &defines)
      : in_(std::string(file)), defines_(defines), logger_(file) {
    Reset();
  }

  // reset lexer status
  void Reset();
  // get next token from input stream
  define::Token NextToken();

  // current logger
  const Logger &logger() const { return logger_; }
  // identifiers
  const std::string &id_val() const { return id_val_; }
  // integer values
  std::uint64_t int_val() const { return int_val_; }
  // floating point values
  double fp_val() const { return fp_val_; }
  // string literals
  const std::string &str_val() const { return str_val_; }
  // character literals
  std::uint8_t char_val() const { return char_val_; }
  // keywords
  define::Keyword key_val() const { return key_val_; }
  // operators
  define::Operator op_val() const { return op_val_; }
  // other characters
  char other_val() const { return other_val_; }

 private:
  // Returns the current input stream.
  std::istream &input() {
    if (expanding_) return expansion_;
    return in_;
  }
  void NextChar() {
    input() >> last_char_;
    if (!expanding_) logger_.IncreaseColPos();
  }
  bool IsEOL() {
    return input().eof() || last_char_ == '\n' || last_char_ == '\r';
  }

  // print error message and return Token::Error
  define::Token LogError(std::string_view message);

  // read escape character from stream
  int ReadEscape();
  // skip spaces in stream
  void SkipSpaces();

  define::Token HandleId();
  define::Token HandleNum();
  define::Token HandleString();
  define::Token HandleChar();
  define::Token HandleOperator();
  define::Token HandleComment();
  define::Token HandleBlockComment();
  define::Token HandleEOL();

  std::ifstream in_;
  // This service is bound to its owner for its entire lifetime.
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  const MacroDefinitions &defines_;
  std::istringstream expansion_;
  bool expanding_ = false;
  char saved_last_char_{};
  Logger saved_logger_;
  Logger logger_;
  char last_char_{};
  // value of token
  std::string id_val_, str_val_;
  std::uint64_t int_val_{};
  double fp_val_{};
  std::uint8_t char_val_{};
  define::Keyword key_val_{};
  define::Operator op_val_{};
  char other_val_{};
};

// pointer to lexer
using LexerPtr = std::shared_ptr<Lexer>;

}  // namespace yulang::front

#endif  // YULANG_FRONT_LEXER_H_
