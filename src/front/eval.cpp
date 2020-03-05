#include "front/eval.h"

#include <type_traits>
#include <cassert>

using namespace yulang::front;
using namespace yulang::define;

// helper macros
#define DO_CALC(op)                                     \
  do {                                                  \
    if constexpr (std::is_same_v<Lhs, std::uint64_t> && \
                  std::is_same_v<Rhs, std::uint64_t>) { \
      if (ast.ast_type()->IsUnsigned()) {               \
        auto ans = static_cast<std::uint64_t>(lhs)      \
            op static_cast<std::uint64_t>(rhs);         \
        return static_cast<std::uint64_t>(ans);         \
      }                                                 \
      else {                                            \
        auto ans = static_cast<std::int64_t>(lhs)       \
            op static_cast<std::int64_t>(rhs);          \
        return static_cast<std::uint64_t>(ans);         \
      }                                                 \
    }                                                   \
    else {                                              \
      return lhs op rhs;                                \
    }                                                   \
  } while (0)
#define DO_INT_CALC(op)                                 \
  do {                                                  \
    if constexpr (std::is_same_v<Lhs, std::uint64_t> && \
                  std::is_same_v<Rhs, std::uint64_t>) { \
      if (ast.ast_type()->IsUnsigned()) {               \
        auto ans = static_cast<std::uint64_t>(lhs)      \
            op static_cast<std::uint64_t>(rhs);         \
        return static_cast<std::uint64_t>(ans);         \
      }                                                 \
      else {                                            \
        auto ans = static_cast<std::int64_t>(lhs)       \
            op static_cast<std::int64_t>(rhs);          \
        return static_cast<std::uint64_t>(ans);         \
      }                                                 \
    }                                                   \
    else {                                              \
      assert(false);                                    \
      return {};                                        \
    }                                                   \
  } while (0)

namespace {

// helper type for the visitor
template <typename T>
struct AlwaysFalse : std::false_type {};

// create a new int/float AST by 'EvalNum'
inline ASTPtr MakeAST(const EvalNum &num, const Logger &log) {
  auto ast = std::visit([](auto &&arg) -> ASTPtr {
    using T = std::decay_t<decltype(arg)>;
    if constexpr (std::is_same_v<T, std::uint64_t>) {
      return std::make_unique<IntAST>(arg);
    }
    else if constexpr (std::is_same_v<T, float> ||
                       std::is_same_v<T, double>) {
      return std::make_unique<FloatAST>(arg);
    }
    else {
      static_assert(AlwaysFalse<T>::value);
      return nullptr;
    }
  }, num);
  ast->set_logger(log);
  return ast;
}

// create a new int AST by value
inline ASTPtr MakeAST(std::uint64_t value, const Logger &log) {
  auto ast = std::make_unique<IntAST>(value);
  ast->set_logger(log);
  return ast;
}

// create a new bool AST by value
inline ASTPtr MakeAST(bool value, const Logger &log) {
  auto ast = std::make_unique<IntAST>(value);
  ast->set_logger(log);
  return ast;
}

// cast 'EvalNum' to boolean
inline bool CastToBool(const EvalNum &num) {
  return std::visit([](auto &&args) { return args != 0; }, num);
}

// cast 'EvalNum' to specific type
inline EvalNum CastToType(const EvalNum &num, const TypePtr &type) {
  assert(type->IsInteger() || type->IsFloat());
  return std::visit([&type](auto &&arg) -> EvalNum {
    if (type->GetSize() == 1) {
      return static_cast<std::uint64_t>(!!arg);
    }
    else if (type->GetSize() == 8) {
      auto ans = type->IsUnsigned() ? static_cast<std::uint8_t>(arg)
                                    : static_cast<std::int8_t>(arg);
      return static_cast<std::uint64_t>(ans);
    }
    else if (type->GetSize() == 16) {
      auto ans = type->IsUnsigned() ? static_cast<std::uint16_t>(arg)
                                    : static_cast<std::int16_t>(arg);
      return static_cast<std::uint64_t>(ans);
    }
    else if (type->GetSize() == 32) {
      if (type->IsInteger()) {
        auto ans = type->IsUnsigned() ? static_cast<std::uint32_t>(arg)
                                      : static_cast<std::int32_t>(arg);
        return static_cast<std::uint64_t>(ans);
      }
      else {
        return static_cast<float>(arg);
      }
    }
    else if (type->GetSize() == 64) {
      if (type->IsInteger()) {
        auto ans = type->IsUnsigned() ? static_cast<std::uint64_t>(arg)
                                      : static_cast<std::int64_t>(arg);
        return static_cast<std::uint64_t>(ans);
      }
      else {
        return static_cast<double>(arg);
      }
    }
    else {
      assert(false);
      return 0ull;
    }
  }, num);
}

// cast value to specific type
inline std::uint64_t CastToType(std::uint64_t num, const TypePtr &type) {
  assert(type->IsInteger());
  if (type->GetSize() == 1) {
    return !!num;
  }
  else if (type->GetSize() == 8) {
    return type->IsUnsigned() ? static_cast<std::uint8_t>(num)
                              : static_cast<std::int8_t>(num);
  }
  else if (type->GetSize() == 16) {
    return type->IsUnsigned() ? static_cast<std::uint16_t>(num)
                              : static_cast<std::int16_t>(num);
  }
  else if (type->GetSize() == 32) {
    return type->IsUnsigned() ? static_cast<std::uint32_t>(num)
                              : static_cast<std::int32_t>(num);
  }
  else if (type->GetSize() == 64) {
    return type->IsUnsigned() ? static_cast<std::uint64_t>(num)
                              : static_cast<std::int64_t>(num);
  }
  else {
    assert(false);
    return 0;
  }
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

std::optional<EvalNum> Evaluator::EvalOn(PropertyAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(VarLetDefAST &ast) {
  for (const auto &i : ast.defs()) i->Eval(*this);
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(FunDefAST &ast) {
  if (ast.body()) ast.body()->Eval(*this);
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(DeclareAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(TypeAliasAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(StructAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(EnumAST &ast) {
  last_enum_name_ = ast.id();
  last_enum_val_ = 0;
  for (const auto &i : ast.defs()) {
    i->Eval(*this);
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(ImportAST &ast) {
  for (const auto &i : ast.defs()) i->Eval(*this);
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(VarElemAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(LetElemAST &ast) {
  // evaluate initial value
  auto val = ast.init()->Eval(*this);
  if (!val) return {};
  // add to environment
  values_->AddItem(ast.id(), val);
  // update AST
  ast.set_init(MakeAST(*val, ast.logger()));
  return val;
}

std::optional<EvalNum> Evaluator::EvalOn(ArgElemAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(StructElemAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(EnumElemAST &ast) {
  // check if has initial expression
  if (ast.expr()) {
    auto val = ast.expr()->Eval(*this);
    auto num = std::get_if<std::uint64_t>(&*val);
    assert(num);
    last_enum_val_ = *num;
    // update AST
    ast.set_expr(MakeAST(*num, ast.logger()));
  }
  last_enum_val_ = CastToType(last_enum_val_, ast.ast_type());
  // add to environment
  if (auto vals = enum_values_->GetItem(last_enum_name_, false)) {
    vals->insert({last_enum_name_, last_enum_val_++});
  }
  else {
    enum_values_->AddItem(last_enum_name_,
                          {{{last_enum_name_, last_enum_val_++}}});
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(BlockAST &ast) {
  auto env = NewEnv();
  bool valid = true;
  for (int i = 0; i < ast.stmts().size(); ++i) {
    // evaluate current statement
    auto val = ast.stmts()[i]->Eval(*this);
    if (val) {
      // update current statement
      ast.set_stmt(i, MakeAST(*val, ast.stmts()[i]->logger()));
    }
    else {
      valid = false;
    }
    // return last value if valid
    if (i == ast.stmts().size() - 1) return valid ? val : std::nullopt;
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(IfAST &ast) {
  // evaluate condition
  bool cond_val;
  auto cond = ast.cond()->Eval(*this);
  if (cond) {
    // get value of condition
    cond_val = CastToBool(*cond);
    // update condition
    ast.set_cond(MakeAST(cond_val, ast.cond()->logger()));
  }
  // evaluate & update true/false part
  auto then_val = ast.then()->Eval(*this);
  if (then_val) {
    ast.set_then(MakeAST(*then_val, ast.then()->logger()));
  }
  auto else_val = ast.else_then() ? ast.else_then()->Eval(*this)
                                  : std::nullopt;
  if (else_val) {
    ast.set_else_then(MakeAST(*else_val, ast.else_then()->logger()));
  }
  // return expression's value
  if (!cond) return {};
  return cond_val ? then_val : else_val;
}

std::optional<EvalNum> Evaluator::EvalOn(WhenAST &ast) {
  auto last = last_when_expr_;
  auto guard = xstl::Guard([this, &last] { last_when_expr_ = last; });
  // evaluate expression
  last_when_expr_ = ast.expr()->Eval(*this);
  if (last_when_expr_) {
    ast.set_expr(MakeAST(*last_when_expr_, ast.expr()->logger()));
  }
  // evaluate elements
  std::optional<EvalNum> ret;
  for (const auto &i : ast.elems()) {
    auto val = i->Eval(*this);
    if (val) ret = val;
  }
  // evaluate 'else' block
  auto else_val = ast.else_then() ? ast.else_then()->Eval(*this)
                                  : std::nullopt;
  if (else_val) {
    ast.set_else_then(MakeAST(*else_val, ast.else_then()->logger()));
  }
  // return expression's value
  if (!last_when_expr_) return {};
  return ret ? ret : else_val;
}

std::optional<EvalNum> Evaluator::EvalOn(WhileAST &ast) {
  // evaluate condition
  auto cond = ast.cond()->Eval(*this);
  if (cond) ast.set_cond(MakeAST(*cond, ast.cond()->logger()));
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()->logger()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(ForInAST &ast) {
  // evaluate expression
  auto expr = ast.expr()->Eval(*this);
  if (expr) ast.set_expr(MakeAST(*expr, ast.expr()->logger()));
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()->logger()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(AsmAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(ControlAST &ast) {
  // evaluate expression
  if (ast.expr()) {
    auto expr = ast.expr()->Eval(*this);
    if (expr) ast.set_expr(MakeAST(*expr, ast.expr()->logger()));
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(WhenElemAST &ast) {
  bool valid = false;
  // evaluate conditions
  for (int i = 0; i < ast.conds().size(); ++i) {
    auto val = ast.conds()[i]->Eval(*this);
    if (val) {
      // update condition
      ast.set_cond(i, MakeAST(*val, ast.conds()[i]->logger()));
      // check if valid
      if (last_when_expr_ && CheckEqual(*val, *last_when_expr_)) {
        valid = true;
      }
    }
  }
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()->logger()));
  return valid ? body : std::nullopt;
}

std::optional<EvalNum> Evaluator::EvalOn(BinaryAST &ast) {
  // evaluate rhs
  auto rhs = ast.rhs()->Eval(*this);
  if (rhs) ast.set_rhs(MakeAST(*rhs, ast.rhs()->logger()));
  // handle by operator
  if (IsOperatorAssign(ast.op())) {
    // do not evaluate rhs, just return null
    return {};
  }
  else {
    // evaluate & update lhs
    auto lhs = ast.lhs()->Eval(*this);
    if (lhs) ast.set_lhs(MakeAST(*lhs, ast.lhs()->logger()));
    // calculate result
    if (lhs && rhs) {
      return std::visit([&ast](auto &&lhs, auto &&rhs) -> EvalNum {
        using Lhs = std::decay_t<decltype(lhs)>;
        using Rhs = std::decay_t<decltype(rhs)>;
        switch (ast.op()) {
          case Operator::Add: return lhs + rhs;
          case Operator::Sub: return lhs - rhs;
          case Operator::Mul: DO_CALC(*);
          case Operator::Div: DO_CALC(/);
          case Operator::Mod: {
            if constexpr (std::is_same_v<Lhs, std::uint64_t> &&
                          std::is_same_v<Rhs, std::uint64_t>) {
              if (ast.ast_type()->IsUnsigned()) {
                auto ans = static_cast<std::uint64_t>(lhs) %
                           static_cast<std::uint64_t>(rhs);
                return static_cast<std::uint64_t>(ans);
              }
              else {
                auto ans = static_cast<std::int64_t>(lhs) %
                           static_cast<std::int64_t>(rhs);
                return static_cast<std::uint64_t>(ans);
              }
            }
            else {
              return std::fmod(lhs, rhs);
            }
          }
          case Operator::Equal: {
            return static_cast<std::uint64_t>(lhs == rhs);
          }
          case Operator::NotEqual: {
            return static_cast<std::uint64_t>(lhs != rhs);
          }
          case Operator::Less: DO_INT_CALC(<);
          case Operator::LessEqual: DO_INT_CALC(<=);
          case Operator::Great: DO_INT_CALC(>);
          case Operator::GreatEqual: DO_INT_CALC(>=);
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
            return static_cast<std::uint64_t>(lhs) <<
                   static_cast<std::uint64_t>(rhs);
          }
          case Operator::Shr: DO_INT_CALC(>>);
          default: assert(false); return {};
        }
      }, *lhs, *rhs);
    }
    return {};
  }
}

std::optional<EvalNum> Evaluator::EvalOn(AccessAST &ast) {
  // try to get id value of expression
  last_id_ = {};
  ast.expr()->Eval(*this);
  auto lhs_id = last_id_;
  last_id_ = {};
  // check if is enumerate
  if (lhs_id) {
    auto ev = enum_values_->GetItem(*lhs_id);
    if (ev) {
      auto it = ev->find(ast.id());
      if (it != ev->end()) return it->second;
    }
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(CastAST &ast) {
  // evaluate & update expression
  auto val = ast.expr()->Eval(*this);
  if (!val) return {};
  ast.set_expr(MakeAST(*val, ast.expr()->logger()));
  // perform type casting
  if (!ast.ast_type()->IsFloat() || !ast.ast_type()->IsInteger()) {
    return {};
  }
  return CastToType(*val, ast.ast_type());
}

std::optional<EvalNum> Evaluator::EvalOn(UnaryAST &ast) {
  using UnaryOp = UnaryAST::UnaryOp;
  // evaluate & update operand
  auto val = ast.opr()->Eval(*this);
  if (val) ast.set_opr(MakeAST(*val, ast.opr()->logger()));
  // caluate the value of AST
  if (ast.op() == UnaryOp::SizeOf) {
    return static_cast<std::uint64_t>(ast.ast_type()->GetSize());
  }
  else if (val && ast.op() != UnaryOp::DeRef &&
           ast.op() != UnaryOp::AddrOf) {
    return std::visit([&ast](auto &&opr) -> EvalNum {
      using T = std::decay_t<decltype(opr)>;
      switch (ast.op()) {
        case UnaryOp::Pos: return +opr;
        case UnaryOp::Neg: return -opr;
        case UnaryOp::LogicNot: return static_cast<std::uint64_t>(!opr);
        case UnaryOp::Not: {
          if constexpr (std::is_same_v<T, std::uint64_t>) {
            return ~opr;
          }
          else {
            assert(false);
            return {};
          }
        }
        default: assert(false); return {};
      }
    }, *val);
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(IndexAST &ast) {
  // evaluate expression
  ast.expr()->Eval(*this);
  // evaluate & update index
  auto val = ast.index()->Eval(*this);
  if (val) ast.set_index(MakeAST(*val, ast.index()->logger()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(FunCallAST &ast) {
  // evaluate expression
  ast.expr()->Eval(*this);
  // evaluate & update arguments
  for (int i = 0; i < ast.args().size(); ++i) {
    auto val = ast.args()[i]->Eval(*this);
    if (val) ast.set_arg(i, MakeAST(*val, ast.args()[i]->logger()));
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(IntAST &ast) {
  auto i32 = static_cast<std::int32_t>(ast.value());
  return static_cast<std::uint64_t>(i32);
}

std::optional<EvalNum> Evaluator::EvalOn(FloatAST &ast) {
  return ast.value();
}

std::optional<EvalNum> Evaluator::EvalOn(CharAST &ast) {
  return static_cast<std::uint64_t>(ast.c());
}

std::optional<EvalNum> Evaluator::EvalOn(IdAST &ast) {
  last_id_ = ast.id();
  return values_->GetItem(ast.id());
}

std::optional<EvalNum> Evaluator::EvalOn(StringAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(BoolAST &ast) {
  return static_cast<std::uint64_t>(ast.value());
}

std::optional<EvalNum> Evaluator::EvalOn(NullAST &ast) {
  return 0ull;
}

std::optional<EvalNum> Evaluator::EvalOn(ValInitAST &ast) {
  // evaluate elements
  for (int i = 0; i < ast.elems().size(); ++i) {
    auto val = ast.elems()[i]->Eval(*this);
    if (val) {
      ast.set_elem(i, MakeAST(*val, ast.elems()[i]->logger()));
    }
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(PrimTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(UserTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(FuncTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(VolaTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(ArrayTypeAST &ast) {
  // evaluate expression
  auto expr = ast.expr()->Eval(*this);
  if (expr) ast.set_expr(MakeAST(*expr, ast.expr()->logger()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(PointerTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(RefTypeAST &ast) {
  return {};
}
