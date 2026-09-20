#include "front/eval.h"

#include <cassert>
#include <cmath>
#include <limits>
#include <type_traits>

#include "define/panic.h"

namespace yulang::front {

using yulang::define::AccessAST;
using yulang::define::ArgElemAST;
using yulang::define::ArrayTypeAST;
using yulang::define::AsmAST;
using yulang::define::ASTPtr;
using yulang::define::BinaryAST;
using yulang::define::BlockAST;
using yulang::define::BoolAST;
using yulang::define::CastAST;
using yulang::define::CharAST;
using yulang::define::ControlAST;
using yulang::define::DeclareAST;
using yulang::define::EnumAST;
using yulang::define::EnumElemAST;
using yulang::define::EvalNum;
using yulang::define::FloatAST;
using yulang::define::ForInAST;
using yulang::define::FunCallAST;
using yulang::define::FuncTypeAST;
using yulang::define::FunDefAST;
using yulang::define::IdAST;
using yulang::define::IfAST;
using yulang::define::ImportAST;
using yulang::define::IndexAST;
using yulang::define::IntAST;
using yulang::define::IsOperatorAssign;
using yulang::define::MakeEnumEnv;
using yulang::define::MakeEvalEnv;
using yulang::define::NullAST;
using yulang::define::Operator;
using yulang::define::PointerTypeAST;
using yulang::define::PrimTypeAST;
using yulang::define::RefTypeAST;
using yulang::define::StringAST;
using yulang::define::StructAST;
using yulang::define::StructElemAST;
using yulang::define::TypeAliasAST;
using yulang::define::TypePtr;
using yulang::define::UnaryAST;
using yulang::define::UserTypeAST;
using yulang::define::ValInitAST;
using yulang::define::VarLetDefAST;
using yulang::define::VarLetElemAST;
using yulang::define::VolaTypeAST;
using yulang::define::WhenAST;
using yulang::define::WhenElemAST;
using yulang::define::WhileAST;

// Operator tokens deliberately appear between operands, not inside parentheses.
// NOLINTBEGIN(bugprone-macro-parentheses)
#define DO_CALC(op)                                                          \
  do {                                                                       \
    if constexpr (std::is_same_v<Lhs, std::uint64_t> &&                      \
                  std::is_same_v<Rhs, std::uint64_t>) {                      \
      if (ast.lhs()->ast_type()->IsUnsigned()) {                             \
        return static_cast<std::uint64_t>(lhs)                               \
            op static_cast<std::uint64_t>(rhs);                              \
      }                                                                      \
      return static_cast<std::uint64_t>(                                     \
          static_cast<std::int64_t>(lhs) op static_cast<std::int64_t>(rhs)); \
    }                                                                        \
    return lhs op rhs;                                                       \
  } while (0)
#define DO_INT_CALC(op)                                                      \
  do {                                                                       \
    if constexpr (std::is_same_v<Lhs, std::uint64_t> &&                      \
                  std::is_same_v<Rhs, std::uint64_t>) {                      \
      if (ast.lhs()->ast_type()->IsUnsigned()) {                             \
        return static_cast<std::uint64_t>(lhs)                               \
            op static_cast<std::uint64_t>(rhs);                              \
      }                                                                      \
      return static_cast<std::uint64_t>(                                     \
          static_cast<std::int64_t>(lhs) op static_cast<std::int64_t>(rhs)); \
    }                                                                        \
    assert(false && "integer-only operator requires integer operands");      \
    return {};                                                               \
  } while (0)
#define DO_COMPARE(op)                                                         \
  do {                                                                         \
    if constexpr (std::is_same_v<Lhs, std::uint64_t> &&                        \
                  std::is_same_v<Rhs, std::uint64_t>) {                        \
      if (!ast.lhs()->ast_type()->IsUnsigned()) {                              \
        return static_cast<std::uint64_t>(                                     \
            static_cast<std::int64_t>(lhs) op static_cast<std::int64_t>(rhs)); \
      }                                                                        \
    }                                                                          \
    return static_cast<std::uint64_t>(lhs op rhs);                             \
  } while (0)
// NOLINTEND(bugprone-macro-parentheses)

namespace {

// helper type for the visitor
template <typename T>
struct AlwaysFalse : std::false_type {};

// create a new AST by 'EvalNum'
inline ASTPtr MakeAST(const EvalNum &num, const ASTPtr &ast) {
  const auto &type = ast->ast_type();
  ASTPtr ret;
  // handle by type of 'ast'
  if (type->IsInteger() || type->IsEnum()) {
    // generate int AST
    const auto *val = std::get_if<std::uint64_t>(&num);
    assert(val);
    ret = std::make_unique<IntAST>(*val);
  } else if (type->IsFloat()) {
    // generate float AST
    // NOTE: there is no loss of precision due to floating point promotion
    //  ref: section 4.6 from n3337
    if (type->GetSize() == 4) {
      const auto *val = std::get_if<float>(&num);
      assert(val);
      ret = std::make_unique<FloatAST>(*val);
    } else {
      const auto *val = std::get_if<double>(&num);
      assert(val);
      ret = std::make_unique<FloatAST>(*val);
    }
  } else if (type->IsBool()) {
    // generate bool AST
    const auto *val = std::get_if<std::uint64_t>(&num);
    assert(val);
    ret = std::make_unique<BoolAST>(*val);
  } else if (type->IsNull()) {
    // generate null AST
    ret = std::make_unique<NullAST>();
  } else {
    assert(false);
  }
  // set additional information
  ret->set_logger(ast->logger());
  if (type->IsRightValue()) {
    ret->set_ast_type(type);
  } else {
    ret->set_ast_type(type->GetValueType(true));
  }
  return ret;
}

// cast 'EvalNum' to boolean
inline bool CastToBool(const EvalNum &num) {
  return std::visit([](auto &&args) { return args != 0; }, num);
}

// Keep integer payloads truncated to their type and sign-extended to 64 bits.
inline std::uint64_t CastToType(std::uint64_t num, const TypePtr &type) {
  assert(type->IsInteger());
  const auto bits = type->GetSize() * 8;
  if (bits < 64) {
    const auto mask = (std::uint64_t{1} << bits) - 1;
    num &= mask;
    if (!type->IsUnsigned() && (num & (std::uint64_t{1} << (bits - 1)))) {
      num |= ~mask;
    }
  }
  return num;
}

// Source signedness is needed because integer EvalNum payloads are unsigned.
inline std::optional<EvalNum> CastToType(const EvalNum &num,
                                         const TypePtr &source,
                                         const TypePtr &type) {
  assert(type->IsInteger() || type->IsBool() || type->IsFloat());
  return std::visit(
      [&source, &type](auto arg) -> std::optional<EvalNum> {
        using T = decltype(arg);
        if (type->IsBool()) return static_cast<std::uint64_t>(!!arg);
        if (type->IsInteger()) {
          std::uint64_t value;
          if constexpr (std::is_same_v<T, std::uint64_t>) {
            value = arg;
          } else {
            // Do not execute an undefined host float-to-integer conversion.
            const auto truncated = std::trunc(static_cast<long double>(arg));
            const auto bits = type->GetSize() * 8;
            const auto upper = std::ldexp(1.0L, bits - !type->IsUnsigned());
            const auto lower = type->IsUnsigned() ? 0.0L : -upper;
            if (!std::isfinite(truncated) || truncated < lower ||
                truncated >= upper) {
              return {};
            }
            value = type->IsUnsigned()
                        ? static_cast<std::uint64_t>(truncated)
                        : static_cast<std::uint64_t>(
                              static_cast<std::int64_t>(truncated));
          }
          return CastToType(value, type);
        }
        if constexpr (std::is_same_v<T, std::uint64_t>) {
          if (!source->IsUnsigned()) {
            const auto value = static_cast<std::int64_t>(arg);
            if (type->GetSize() == 4) return static_cast<float>(value);
            return static_cast<double>(value);
          }
        }
        if (type->GetSize() == 4) return static_cast<float>(arg);
        return static_cast<double>(arg);
      },
      num);
}

// check if two 'EvalNum' are equal
inline bool CheckEqual(const EvalNum &lhs, const EvalNum &rhs) {
  return std::visit([](auto &&l, auto &&r) { return l == r; }, lhs, rhs);
}

}  // namespace

xstl::Guard Evaluator::NewEnv() {
  values_ = MakeEvalEnv(values_);
  enum_values_ = MakeEnumEnv(enum_values_);
  return xstl::Guard([this] {
    values_ = values_->outer();
    enum_values_ = enum_values_->outer();
  });
}

std::optional<EvalNum> Evaluator::EvalOn(VarLetDefAST &ast) {
  for (const auto &i : ast.defs()) i->Eval(*this);
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(FunDefAST &ast) {
  const auto env = NewEnv();
  for (const auto &arg : ast.args()) arg->Eval(*this);
  if (ast.body()) ast.body()->Eval(*this);
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(DeclareAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(TypeAliasAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(StructAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(EnumAST &ast) {
  last_enum_name_ = ast.id();
  last_enum_val_ = 0;
  for (const auto &i : ast.defs()) {
    i->Eval(*this);
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(ImportAST & /*ast*/) {
  // do nothing, since imported ASTs are evaluated
  // in Analyzer::AnalyzeOn(ImportAST &)
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(VarLetElemAST &ast) {
  // do not evaluate reference
  if (ast.ast_type()->IsReference()) {
    Mask(ast.id());
    return {};
  }
  // evaluate initial value
  auto val = ast.init() ? ast.init()->Eval(*this) : std::nullopt;
  // Initializers still see the outer binding; the new binding then masks it.
  values_->AccessItem(ast.id()) = ast.is_var() ? std::nullopt : val;
  if (!val) return {};
  // update AST
  ast.set_init(MakeAST(*val, ast.init()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(ArgElemAST &ast) {
  Mask(ast.id());
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(StructElemAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(EnumElemAST &ast) {
  // check if has initial expression
  if (ast.expr()) {
    auto val = ast.expr()->Eval(*this);
    if (!val) {
      ast.logger().LogError("enumeration value must be a constant integer");
      return {};
    }
    const auto *num = std::get_if<std::uint64_t>(&*val);
    if (!num) {
      ast.logger().LogError("enumeration value must be a constant integer");
      return {};
    }
    last_enum_val_ = *num;
    // update AST
    ast.set_expr(MakeAST(*num, ast.expr()));
  }
  last_enum_val_ = CastToType(last_enum_val_, ast.ast_type());
  // add to environment
  if (enum_values_->GetItem(last_enum_name_, false)) {
    auto &val = enum_values_->AccessItem(last_enum_name_);
    if (!val) PANIC("missing enumeration environment");
    val->insert({ast.id(), last_enum_val_++});
  } else {
    enum_values_->AddItem(last_enum_name_, {{{ast.id(), last_enum_val_++}}});
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(BlockAST &ast) {
  const auto env = NewEnv();
  bool valid = true;
  for (std::size_t i = 0; i < ast.stmts().size(); ++i) {
    // evaluate current statement
    auto val = ast.stmts()[i]->Eval(*this);
    if (val) {
      // update current statement
      ast.set_stmt(i, MakeAST(*val, ast.stmts()[i]));
    } else {
      valid = false;
    }
    // return last value if valid
    if (i == ast.stmts().size() - 1) return valid ? val : std::nullopt;
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(IfAST &ast) {
  // evaluate condition
  std::uint64_t cond_val = 0;
  auto cond = ast.cond()->Eval(*this);
  if (cond) {
    // get value of condition
    cond_val = CastToBool(*cond);
    // update condition
    ast.set_cond(MakeAST(cond_val, ast.cond()));
  }
  // evaluate & update true/false part
  auto then_val = ast.then()->Eval(*this);
  if (then_val) {
    ast.set_then(MakeAST(*then_val, ast.then()));
  }
  auto else_val = ast.else_then() ? ast.else_then()->Eval(*this) : std::nullopt;
  if (else_val) {
    ast.set_else_then(MakeAST(*else_val, ast.else_then()));
  }
  // return expression's value
  if (!cond) return {};
  return cond_val ? then_val : else_val;
}

std::optional<EvalNum> Evaluator::EvalOn(WhenAST &ast) {
  auto last = last_when_expr_;
  auto last_match = last_when_match_;
  const auto guard = xstl::Guard([this, &last, &last_match] {
    last_when_expr_ = last;
    last_when_match_ = last_match;
  });
  // evaluate expression
  last_when_expr_ = ast.expr()->Eval(*this);
  if (last_when_expr_) {
    ast.set_expr(MakeAST(*last_when_expr_, ast.expr()));
  }
  // evaluate elements
  std::optional<EvalNum> ret;
  bool selection_known = true;
  bool selected = false;
  for (const auto &i : ast.elems()) {
    const auto val = i->Eval(*this);
    if (selection_known && !selected) {
      if (!last_when_match_) {
        selection_known = false;
      } else if (*last_when_match_) {
        selected = true;
        ret = val;
      }
    }
  }
  // evaluate 'else' block
  auto else_val = ast.else_then() ? ast.else_then()->Eval(*this) : std::nullopt;
  if (else_val) {
    ast.set_else_then(MakeAST(*else_val, ast.else_then()));
  }
  // return expression's value
  if (!last_when_expr_ || !selection_known || ast.ast_type()->IsVoid()) {
    return {};
  }
  return selected ? ret : else_val;
}

std::optional<EvalNum> Evaluator::EvalOn(WhileAST &ast) {
  // evaluate condition
  auto cond = ast.cond()->Eval(*this);
  if (cond) ast.set_cond(MakeAST(*cond, ast.cond()));
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(ForInAST &ast) {
  const auto env = NewEnv();
  // evaluate expression
  auto expr = ast.expr()->Eval(*this);
  if (expr) ast.set_expr(MakeAST(*expr, ast.expr()));
  Mask(ast.id());
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(AsmAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(ControlAST &ast) {
  // evaluate expression
  if (ast.expr()) {
    auto expr = ast.expr()->Eval(*this);
    if (expr) ast.set_expr(MakeAST(*expr, ast.expr()));
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(WhenElemAST &ast) {
  std::optional<bool> matched = false;
  // evaluate conditions
  for (std::size_t i = 0; i < ast.conds().size(); ++i) {
    auto val = ast.conds()[i]->Eval(*this);
    if (val) {
      // update condition
      ast.set_cond(i, MakeAST(*val, ast.conds()[i]));
    }
    // Conditions are tested in order. An unknown condition before a match
    // must remain in the program, even if a later condition matches.
    if (matched && !*matched) {
      matched = val && last_when_expr_
                    ? std::optional<bool>(CheckEqual(*val, *last_when_expr_))
                    : std::nullopt;
    }
  }
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()));
  last_when_match_ = matched;
  return matched && *matched ? body : std::nullopt;
}

// Keep the exhaustive type/operator dispatch together for semantic review.
// std::visit instantiates floating alternatives too; the operator is selected
// at runtime, so its integer-only assertion cannot become a static_assert.
// NOLINTBEGIN(misc-static-assert)
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
std::optional<EvalNum> Evaluator::EvalOn(BinaryAST &ast) {
  // evaluate rhs
  auto rhs = ast.rhs()->Eval(*this);
  if (rhs) ast.set_rhs(MakeAST(*rhs, ast.rhs()));
  // handle by operator
  if (IsOperatorAssign(ast.op())) {
    // do not evaluate rhs, just return null
    return {};
  }
  // evaluate & update lhs
  auto lhs = ast.lhs()->Eval(*this);
  if (lhs) ast.set_lhs(MakeAST(*lhs, ast.lhs()));
  // calculate result
  if (lhs && rhs) {
    auto result = std::visit(
        // Keep the exhaustive type/operator dispatch together for semantic
        // review. NOLINTNEXTLINE(readability-function-cognitive-complexity)
        [&ast](auto &&lhs, auto &&rhs) -> std::optional<EvalNum> {
          using Lhs = std::decay_t<decltype(lhs)>;
          using Rhs = std::decay_t<decltype(rhs)>;
          if constexpr (std::is_same_v<Lhs, std::uint64_t> &&
                        std::is_same_v<Rhs, std::uint64_t>) {
            if (ast.op() == Operator::Div || ast.op() == Operator::Mod) {
              if (!rhs) return {};
              if (!ast.lhs()->ast_type()->IsUnsigned() &&
                  static_cast<std::int64_t>(lhs) ==
                      std::numeric_limits<std::int64_t>::min() &&
                  static_cast<std::int64_t>(rhs) == -1) {
                return {};
              }
            }
            if ((ast.op() == Operator::Shl || ast.op() == Operator::Shr) &&
                rhs >= ast.lhs()->ast_type()->GetSize() * 8) {
              return {};
            }
          }
          switch (ast.op()) {
            case Operator::Add:
              return lhs + rhs;
            case Operator::Sub:
              return lhs - rhs;
            case Operator::Mul:
              return lhs * rhs;
            case Operator::Div:
              DO_CALC(/);
            case Operator::Mod: {
              if constexpr (std::is_same_v<Lhs, std::uint64_t> &&
                            std::is_same_v<Rhs, std::uint64_t>) {
                if (ast.ast_type()->IsUnsigned()) {
                  auto ans = static_cast<std::uint64_t>(lhs) %
                             static_cast<std::uint64_t>(rhs);
                  return ans;
                }
                auto ans = static_cast<std::int64_t>(lhs) %
                           static_cast<std::int64_t>(rhs);
                return static_cast<std::uint64_t>(ans);

              } else {
                return std::fmod(lhs, rhs);
              }
            }
            case Operator::Equal: {
              return static_cast<std::uint64_t>(lhs == rhs);
            }
            case Operator::NotEqual: {
              return static_cast<std::uint64_t>(lhs != rhs);
            }
            case Operator::Less:
              DO_COMPARE(<);
            case Operator::LessEqual:
              DO_COMPARE(<=);
            case Operator::Great:
              DO_COMPARE(>);
            case Operator::GreatEqual:
              DO_COMPARE(>=);
            case Operator::LogicAnd: {
              return static_cast<std::uint64_t>(lhs && rhs);
            }
            case Operator::LogicOr: {
              return static_cast<std::uint64_t>(lhs || rhs);
            }
            case Operator::And: {
              return static_cast<std::uint64_t>(lhs) &
                     static_cast<std::uint64_t>(rhs);
            }
            case Operator::Or: {
              return static_cast<std::uint64_t>(lhs) |
                     static_cast<std::uint64_t>(rhs);
            }
            case Operator::Xor: {
              return static_cast<std::uint64_t>(lhs) ^
                     static_cast<std::uint64_t>(rhs);
            }
            case Operator::Shl: {
              return static_cast<std::uint64_t>(lhs)
                     << static_cast<std::uint64_t>(rhs);
            }
            case Operator::Shr:
              // Signed YuLang right shift intentionally preserves the sign bit.
              // NOLINTNEXTLINE(bugprone-signed-bitwise)
              DO_INT_CALC(>>);
            default:
              assert(false);
              return {};
          }
        },
        *lhs, *rhs);
    if (result && ast.ast_type()->IsInteger()) {
      return CastToType(std::get<std::uint64_t>(*result), ast.ast_type());
    }
    return result;
  }
  return {};
}

// NOLINTEND(misc-static-assert)

std::optional<EvalNum> Evaluator::EvalOn(AccessAST &ast) {
  // check if is enumerate
  const auto type = ast.expr()->ast_type();
  if (type->IsEnum()) {
    auto ev = enum_values_->GetItem(type->GetTypeId());
    if (ev) {
      const auto it = ev->find(ast.id());
      if (it != ev->end()) return it->second;
    }
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(CastAST &ast) {
  // evaluate & update expression
  auto val = ast.expr()->Eval(*this);
  if (!val) return {};
  ast.set_expr(MakeAST(*val, ast.expr()));
  // perform type casting
  if (!ast.ast_type()->IsInteger() && !ast.ast_type()->IsBool() &&
      !ast.ast_type()->IsFloat()) {
    return {};
  }
  return CastToType(*val, ast.expr()->ast_type(), ast.ast_type());
}

std::optional<EvalNum> Evaluator::EvalOn(UnaryAST &ast) {
  using UnaryOp = UnaryAST::UnaryOp;
  // evaluate & update operand
  auto val = ast.opr()->Eval(*this);
  if (val && ast.op() != UnaryOp::AddrOf) {
    ast.set_opr(MakeAST(*val, ast.opr()));
  }
  // caluate the value of AST
  if (ast.op() == UnaryOp::SizeOf) {
    return static_cast<std::uint64_t>(ast.opr()->ast_type()->GetSize());
  }
  if (val && ast.op() != UnaryOp::DeRef && ast.op() != UnaryOp::AddrOf) {
    auto result = std::visit(
        [&ast](auto &&opr) -> EvalNum {
          using T = std::decay_t<decltype(opr)>;
          switch (ast.op()) {
            case UnaryOp::Pos:
              return +opr;
            case UnaryOp::Neg:
              return -opr;
            case UnaryOp::LogicNot:
              return static_cast<std::uint64_t>(!opr);
            case UnaryOp::Not: {
              if constexpr (std::is_same_v<T, std::uint64_t>) {
                return ~opr;
              } else {
                assert(false);
                return {};
              }
            }
            default:
              assert(false);
              return {};
          }
        },
        *val);
    if (ast.ast_type()->IsInteger()) {
      return CastToType(std::get<std::uint64_t>(result), ast.ast_type());
    }
    return result;
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(IndexAST &ast) {
  // evaluate expression
  ast.expr()->Eval(*this);
  // evaluate & update index
  auto val = ast.index()->Eval(*this);
  if (val) ast.set_index(MakeAST(*val, ast.index()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(FunCallAST &ast) {
  // evaluate expression
  ast.expr()->Eval(*this);
  // evaluate & update arguments
  for (std::size_t i = 0; i < ast.args().size(); ++i) {
    auto val = ast.args()[i]->Eval(*this);
    if (val) ast.set_arg(i, MakeAST(*val, ast.args()[i]));
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(IntAST &ast) {
  // NOTE: integer literals can represent integers wider than 32-bit
  return ast.value();
}

std::optional<EvalNum> Evaluator::EvalOn(FloatAST &ast) {
  if (ast.ast_type()->GetSize() == 4) return static_cast<float>(ast.value());
  return ast.value();
}

std::optional<EvalNum> Evaluator::EvalOn(CharAST &ast) {
  return static_cast<std::uint64_t>(ast.c());
}

std::optional<EvalNum> Evaluator::EvalOn(IdAST &ast) {
  return values_->GetItem(ast.id());
}

std::optional<EvalNum> Evaluator::EvalOn(StringAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(BoolAST &ast) {
  return static_cast<std::uint64_t>(ast.value());
}

std::optional<EvalNum> Evaluator::EvalOn(NullAST & /*ast*/) {
  return static_cast<std::uint64_t>(0);
}

std::optional<EvalNum> Evaluator::EvalOn(ValInitAST &ast) {
  // evaluate elements
  for (std::size_t i = 0; i < ast.elems().size(); ++i) {
    auto val = ast.elems()[i]->Eval(*this);
    if (val) {
      ast.set_elem(i, MakeAST(*val, ast.elems()[i]));
    }
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(PrimTypeAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(UserTypeAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(FuncTypeAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(VolaTypeAST & /*ast*/) { return {}; }

std::optional<EvalNum> Evaluator::EvalOn(ArrayTypeAST &ast) {
  // evaluate expression
  auto expr = ast.expr()->Eval(*this);
  if (expr) ast.set_expr(MakeAST(*expr, ast.expr()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(PointerTypeAST & /*ast*/) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(RefTypeAST & /*ast*/) { return {}; }

}  // namespace yulang::front
