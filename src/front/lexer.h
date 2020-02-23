#ifndef YULANG_FRONT_LEXER_H_
#define YULANG_FRONT_LEXER_H_

#include <istream>
#include <string_view>
#include <string>
#include <cstdint>
#include <cstddef>
#include <cassert>

namespace yulang::front {

enum class Token {
  Error, End,
  Id, Int, Float, String, Char,
  Keyword, Operator,
  Other,
};

enum class Keyword {
  // var, let, declare, type, as, sizeof
  Var, Let, Declare, Type, As, SizeOf,
  // struct, enum, asm
  Struct, Enum, Asm,
  // i8, i16, i32, i64
  Int8, Int16, Int32, Int64,
  // u8, u16, u32, u64
  UInt8, UInt16, UInt32, UInt64,
  // bool, f32, f64
  Bool, Float32, Float64,
  // true, false, null
  True, False, Null,
  // import, public, extern, volatile
  Import, Public, Extern, Volatile,
  // if, else, when, while, for, in
  If, Else, When, While, For, In,
  // break, continue, return
  Break, Continue, Return,
};

enum class Operator {
  // +, -, *, /, %
  Add, Sub, Mul, Div, Mod,
  // ==, !=, <, <=, >, >=
  Equal, NotEqual, Less, LessEqual, Great, GreatEqual,
  // &&, ||, !
  LogicAnd, LogicOr, LogicNot,
  // &, |, ~, ^, <<, >>
  And, Or, Not, Xor, Shl, Shr,
  // .
  Access,
  // =, +=, -=, *=, /=, %=
  Assign, AssAdd, AssSub, AssMul, AssDiv, AssMod,
  // &=, |=, ^=, <<=, >>=
  AssAnd, AssOr, AssXor, AssShl, AssShr,
};

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
  Token NextToken();

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
  Keyword key_val() const { return key_val_; }
  // operators
  Operator op_val() const { return op_val_; }
  // other characters
  char other_val() const { return other_val_; }

 private:
  void NextChar() { in_ >> last_char_; }
  bool IsEOL() {
    return in_.eof() || last_char_ == '\n' || last_char_ == '\r';
  }

  // print error message and return Token::Error
  Token PrintError(std::string_view message);

  // read escape character from stream
  int ReadEscape();

  Token HandleId();
  Token HandleNum();
  Token HandleString();
  Token HandleChar();
  Token HandleOperator();
  Token HandleComment();
  Token HandleBlockComment();
  Token HandleEOL();

  std::istream &in_;
  std::size_t line_pos_, error_num_;
  char last_char_;
  // value of token
  std::string id_val_, str_val_;
  std::uint64_t int_val_;
  double fp_val_;
  std::uint8_t char_val_;
  Keyword key_val_;
  Operator op_val_;
  char other_val_;
};

// check if operator is assign ('=', '+=', '-=', ...)
inline bool IsOperatorAssign(Operator op) {
  return static_cast<int>(op) >= static_cast<int>(Operator::Assign);
}

// get de-assigned operator ('+=' -> '+', '-=' -> '-', ...)
inline Operator GetDeAssignedOp(Operator op) {
  switch (op) {
    case Operator::AssAdd: return Operator::Add;
    case Operator::AssSub: return Operator::Sub;
    case Operator::AssMul: return Operator::Mul;
    case Operator::AssDiv: return Operator::Div;
    case Operator::AssMod: return Operator::Mod;
    case Operator::AssAnd: return Operator::And;
    case Operator::AssOr: return Operator::Or;
    case Operator::AssXor: return Operator::Xor;
    case Operator::AssShl: return Operator::Shl;
    case Operator::AssShr: return Operator::Shr;
    default: assert(false); return Operator::Assign;
  }
}

}  // namespace yulang::front

#endif  // YULANG_FRONT_LEXER_H_
