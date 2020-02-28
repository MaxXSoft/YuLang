#ifndef YULANG_DEFINE_TOKEN_H_
#define YULANG_DEFINE_TOKEN_H_

#include <cassert>

// all supported keywords
#define YULANG_KEYWORDS(e) \
  e(Var, "var") e(Let, "let") e(Def, "def") e(Declare, "declare") \
  e(Type, "type") e(As, "as") e(SizeOf, "sizeof") \
  e(Struct, "struct") e(Enum, "enum") e(Asm, "asm") \
  e(Int8, "i8") e(Int16, "i16") e(Int32, "i32") e(Int64, "i64") \
  e(UInt8, "u8") e(UInt16, "u16") e(UInt32, "u32") e(UInt64, "u64") \
  e(Bool, "bool") e(Float32, "f32") e(Float64, "f64") \
  e(True, "true") e(False, "false") e(Null, "null") \
  e(Import, "import") e(Public, "public") \
  e(Extern, "extern") e(Volatile, "volatile") \
  e(If, "if") e(Else, "else") e(When, "when") e(While, "while") \
  e(For, "for") e(In, "in") \
  e(Break, "break") e(Continue, "continue") e(Return, "return")

// all supported operators
#define YULANG_OPERATORS(e) \
  e(Add, "+", 90) e(Sub, "-", 90) e(Mul, "*", 100) e(Div, "/", 100) \
  e(Mod, "%", 100) e(Equal, "==", 60) e(NotEqual, "!=", 60) \
  e(Less, "<", 70) e(LessEqual, "<=", 70) e(Great, ">", 70) \
  e(GreatEqual, ">=", 70) e(LogicAnd, "&&", 20) e(LogicOr, "||", 10) \
  e(LogicNot, "!", -1) e(And, "&", 50) e(Or, "|", 30) e(Not, "~", -1) \
  e(Xor, "^", 40) e(Shl, "<<", 80) e(Shr, ">>", 80) e(Access, ".", 110) \
  e(Assign, "=", 0) e(AssAdd, "+=", 0) e(AssSub, "-=", 0) \
  e(AssMul, "*=", 0) e(AssDiv, "/=", 0) e(AssMod, "%=", 0) \
  e(AssAnd, "&=", 0) e(AssOr, "|=", 0) e(AssXor, "^=", 0) \
  e(AssShl, "<<=", 0) e(AssShr, ">>=", 0)

// expand first element to comma-separated list
#define YULANG_EXPAND_FIRST(i, ...)       i,
// expand second element to comma-separated list
#define YULANG_EXPAND_SECOND(i, j, ...)   j,
// expand third element to comma-separated list
#define YULANG_EXPAND_THIRD(i, j, k, ...) k,

namespace yulang::define {

enum class Token {
  Error, End, EOL,
  Id, Int, Float, String, Char,
  Keyword, Operator,
  Other,
};

enum class Keyword { YULANG_KEYWORDS(YULANG_EXPAND_FIRST) };
enum class Operator { YULANG_OPERATORS(YULANG_EXPAND_FIRST) };

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

}  // namespace yulang::define

#endif  // YULANG_DEFINE_TOKEN_H_
