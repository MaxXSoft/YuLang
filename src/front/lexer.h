#ifndef YULANG_FRONT_LEXER_H_
#define YULANG_FRONT_LEXER_H_

#include <istream>
#include <string_view>
#include <string>
#include <cstdint>
#include <cstddef>
#include <cassert>

#include "define/token.h"

namespace yulang::front {

class Lexer {
 public:
  Lexer(std::istream &in) : in_(in) { Reset(); }

  // reset lexer status
  void Reset() {
    line_pos_ = 1;
    error_num_ = 0;
    last_char_ = ' ';
    in_ >> std::noskipws;
  }

  // check if next token is end of line, and skip it if true
  bool SkipEOL();
  // get next token from input stream
  define::Token NextToken();

  // current line position
  std::size_t line_pos() const { return line_pos_; }
  // current error count
  std::size_t error_num() const { return error_num_; }
  // identifiers
  std::string_view id_val() const { return id_val_; }
  // integer values
  std::uint64_t int_val() const { return int_val_; }
  // floating point values
  double fp_val() const { return fp_val_; }
  // string literals
  std::string_view str_val() const { return str_val_; }
  // character literals
  std::uint8_t char_val() const { return char_val_; }
  // keywords
  define::Keyword key_val() const { return key_val_; }
  // operators
  define::Operator op_val() const { return op_val_; }
  // other characters
  char other_val() const { return other_val_; }

 private:
  void NextChar() { in_ >> last_char_; }
  bool IsEOL() {
    return in_.eof() || last_char_ == '\n' || last_char_ == '\r';
  }

  // print error message and return Token::Error
  define::Token PrintError(std::string_view message);

  // read escape character from stream
  int ReadEscape();

  define::Token HandleId();
  define::Token HandleNum();
  define::Token HandleString();
  define::Token HandleChar();
  define::Token HandleOperator();
  define::Token HandleComment();
  define::Token HandleBlockComment();
  define::Token HandleEOL();

  std::istream &in_;
  std::size_t line_pos_, error_num_;
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

}  // namespace yulang::front

#endif  // YULANG_FRONT_LEXER_H_
