#ifndef YULANG_FRONT_LEXER_H_
#define YULANG_FRONT_LEXER_H_

#include <fstream>
#include <utility>
#include <string_view>
#include <string>
#include <memory>
#include <cstdint>
#include <cassert>

#include "define/token.h"
#include "front/logger.h"

namespace yulang::front {

class Lexer {
 public:
  Lexer(std::string_view file) : in_(std::string(file)), logger_(file) {
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
  void NextChar() {
    in_ >> last_char_;
    logger_.IncreaseColPos();
  }
  bool IsEOL() {
    return in_.eof() || last_char_ == '\n' || last_char_ == '\r';
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
  Logger logger_;
  char last_char_;
  // value of token
  std::string id_val_, str_val_;
  std::uint64_t int_val_;
  double fp_val_;
  std::uint8_t char_val_;
  define::Keyword key_val_;
  define::Operator op_val_;
  char other_val_;
};

// pointer to lexer
using LexerPtr = std::shared_ptr<Lexer>;

}  // namespace yulang::front

#endif  // YULANG_FRONT_LEXER_H_
