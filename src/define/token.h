#ifndef YULANG_DEFINE_TOKEN_H_
#define YULANG_DEFINE_TOKEN_H_

#include <cassert>

namespace yulang::define {

enum class Token {
  Error, End,
  Id, Int, Float, String, Char,
  Keyword, Operator,
  Other,
};

enum class Keyword {
  // var, let, def, declare, type, as, sizeof
  Var, Let, Def, Declare, Type, As, SizeOf,
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
