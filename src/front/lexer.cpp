#include "front/lexer.h"

#include <array>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <limits>
#include <string_view>

#include "define/token.h"

namespace yulang::front {

using yulang::define::Keyword;
using yulang::define::Operator;
using yulang::define::Token;

namespace {

enum class NumberType : std::uint8_t { Normal, Hex, Bin, Float };

const std::array kKeywords = {YULANG_KEYWORDS(YULANG_EXPAND_SECOND)};
const std::array kOperators = {YULANG_OPERATORS(YULANG_EXPAND_SECOND)};

// get index of a string in string array
template <typename T, std::size_t N>
int GetIndex(const char *str, const std::array<T, N> &str_array) {
  static_assert(N <= static_cast<std::size_t>(std::numeric_limits<int>::max()));
  for (std::size_t i = 0; i < N; ++i) {
    if (!std::strcmp(str, str_array[i])) return static_cast<int>(i);
  }
  return -1;
}

bool IsOperatorChar(char c) {
  constexpr std::string_view op_chars = "`~!@#$%^&*-=+\\|:<.>/?";
  return op_chars.find(c) != std::string_view::npos;
}

}  // namespace

Token Lexer::LogError(std::string_view message) {
  logger_.LogError(message);
  return Token::Error;
}

int Lexer::ReadEscape() {
  // eat '\'
  NextChar();
  if (IsEOL()) return -1;
  switch (last_char_) {
    case 'a':
      return '\a';
    case 'b':
      return '\b';
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\\':
      return '\\';
    case '\'':
      return '\'';
    case '"':
      return '"';
    case '0':
      return '\0';
    case 'x': {
      std::array<char, 3> hex{};
      char *end_pos = nullptr;
      // read 2 hex digits
      for (int i = 0; i < 2; ++i) {
        NextChar();
        if (IsEOL()) return -1;
        hex[i] = last_char_;
      }
      // convert to character
      const auto ret = std::strtol(hex.data(), &end_pos, 16);
      return *end_pos ? -1 : static_cast<int>(ret);
    }
    default:
      return -1;
  }
}

void Lexer::SkipSpaces() {
  while (!IsEOL() && std::isspace(last_char_)) NextChar();
}

Token Lexer::HandleId() {
  const auto location = logger_;
  // read string
  std::string id;
  do {
    id += last_char_;
    NextChar();
  } while (!IsEOL() && (std::isalnum(last_char_) || last_char_ == '_'));
  // check if string is keyword
  int const index = GetIndex(id.c_str(), kKeywords);
  if (index < 0) {
    if (!expanding_) {
      // handle macro expansion
      const auto it = defines_.find(id);
      if (it != defines_.end()) {
        saved_last_char_ = last_char_;
        saved_logger_ = logger_;
        logger_ = location;
        expansion_.str(it->second);
        expansion_.clear();
        expansion_ >> std::noskipws;
        expanding_ = true;
        last_char_ = ' ';
        return NextToken();
      }
    }
    id_val_ = id;
    return Token::Id;
  }
  key_val_ = static_cast<Keyword>(index);
  return Token::Keyword;
}

Token Lexer::HandleNum() {
  std::string num;
  NumberType num_type = NumberType::Normal;
  // check if is hexadecimal/binary/floating-point number
  if (last_char_ == '0') {
    NextChar();
    switch (last_char_) {
      // hexadecimal
      case 'x':
      case 'X':
        num_type = NumberType::Hex;
        break;
      // binary
      case 'b':
      case 'B':
        num_type = NumberType::Bin;
        break;
      // floating point
      case '.':
        num = "0.";
        num_type = NumberType::Float;
        break;
      default: {
        if (IsEOL() || !std::isdigit(last_char_)) {
          // just zero
          int_val_ = 0;
          return Token::Int;
        }
        return LogError("invalid number literal");

        break;
      }
    }
    NextChar();
  }
  // read number string
  while (!IsEOL() && (std::isxdigit(last_char_) || last_char_ == '.' ||
                      last_char_ == '_')) {
    if (num_type != NumberType::Float && last_char_ == '.') {
      num_type = NumberType::Float;
    }
    if (last_char_ != '_') num += last_char_;
    NextChar();
  }
  // convert to number
  char *end_pos = nullptr;
  Token ret{Token::Error};
  switch (num_type) {
    case NumberType::Hex: {
      int_val_ = std::strtoull(num.c_str(), &end_pos, 16);
      ret = Token::Int;
      break;
    }
    case NumberType::Bin: {
      int_val_ = std::strtoull(num.c_str(), &end_pos, 2);
      ret = Token::Int;
      break;
    }
    case NumberType::Normal: {
      int_val_ = std::strtoull(num.c_str(), &end_pos, 10);
      ret = Token::Int;
      break;
    }
    case NumberType::Float: {
      fp_val_ = std::strtod(num.c_str(), &end_pos);
      ret = Token::Float;
      break;
    }
    default:;
  }
  // check if conversion is valid
  return *end_pos ? LogError("invalid number literal") : ret;
}

Token Lexer::HandleString() {
  std::string str;
  // start with quotes
  NextChar();
  while (last_char_ != '"') {
    if (last_char_ == '\\') {
      // read escape char
      int const ret = ReadEscape();
      if (ret < 0) return LogError("invalid escape character");
      str += static_cast<char>(ret);
    } else {
      str += last_char_;
    }
    NextChar();
    if (IsEOL()) return LogError("expected '\"'");
  }
  // eat right quotation mark
  NextChar();
  str_val_ = str;
  return Token::String;
}

Token Lexer::HandleChar() {
  // start with quotes
  NextChar();
  if (IsEOL()) return LogError("expected character literal");
  if (last_char_ == '\\') {
    // read escape char
    int const ret = ReadEscape();
    if (ret < 0) return LogError("invalid escape character");
    char_val_ = ret;
  } else {
    char_val_ = last_char_;
  }
  NextChar();
  // check & eat right quotation mark
  if (IsEOL() || last_char_ != '\'') return LogError("expected \"'\"");
  NextChar();
  return Token::Char;
}

Token Lexer::HandleOperator() {
  std::string op;
  // read first char
  op += last_char_;
  NextChar();
  // check if is comment
  if ((op[0] == '/') && (!IsEOL())) {
    switch (last_char_) {
      case '/':
        return HandleComment();
      case '*':
        return HandleBlockComment();
      default:
        break;
    }
  }

  // read rest chars
  while (!IsEOL() && IsOperatorChar(last_char_)) {
    op += last_char_;
    NextChar();
  }
  // check if operator is valid
  int const index = GetIndex(op.c_str(), kOperators);
  if (index < 0) {
    // treat unknown operator as identifier
    id_val_ = op;
    return Token::Id;
  }
  op_val_ = static_cast<Operator>(index);
  return Token::Operator;
}

Token Lexer::HandleComment() {
  // eat '/'
  NextChar();
  while (!IsEOL()) NextChar();
  return NextToken();
}

Token Lexer::HandleBlockComment() {
  // eat '*'
  NextChar();
  // read until there is '*/' in stream
  bool star = false;
  while (!input().eof() && (!star || last_char_ != '/')) {
    star = last_char_ == '*';
    if (IsEOL() && !input().eof() && !expanding_) logger_.IncreaseLinePos();
    NextChar();
  }
  // check unclosed block comment
  if (input().eof()) return LogError("comment unclosed at EOF");
  // eat '/'
  NextChar();
  return NextToken();
}

Token Lexer::HandleEOL() {
  do {
    if (!expanding_) logger_.IncreaseLinePos();
    NextChar();
  } while (IsEOL() && !input().eof());
  return Token::EOL;
}

void Lexer::Reset() {
  expanding_ = false;
  expansion_.str("");
  expansion_.clear();
  logger_.Reset();
  last_char_ = ' ';
  // check if file was opened
  if (!in_.is_open()) {
    LogError("failed to open file");
  } else {
    // reset status of file stream
    in_.clear();
    in_.seekg(0, std::ios::beg);
    in_ >> std::noskipws;
  }
}

Token Lexer::NextToken() {
  // A replacement ends at a token boundary, never in NextChar(). Restore
  // the already-read source character without joining tokens across streams.
  for (;;) {
    SkipSpaces();
    if (!input().eof()) break;
    if (!expanding_) return Token::End;
    expanding_ = false;
    last_char_ = saved_last_char_;
    logger_ = saved_logger_;
  }
  // id or keyword
  if (std::isalpha(last_char_) || last_char_ == '_') return HandleId();
  // number
  if (std::isdigit(last_char_)) return HandleNum();
  // string
  if (last_char_ == '"') return HandleString();
  // character
  if (last_char_ == '\'') return HandleChar();
  // operator or id
  if (IsOperatorChar(last_char_)) return HandleOperator();
  // end of line (line break or delimiter)
  if (IsEOL()) return HandleEOL();
  if (last_char_ == ';') {
    NextChar();
    return Token::EOL;
  }
  // other characters
  other_val_ = last_char_;
  NextChar();
  return Token::Other;
}

}  // namespace yulang::front
