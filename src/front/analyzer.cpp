#include "front/analyzer.h"

#include <type_traits>
#include <cassert>

#include "front/logger.h"
#include "xstl/guard.h"

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
      static_assert(false);
      return nullptr;
    }
  }, num);
  ast->set_logger(log);
  return ast;
}

// create a new int/float/bool AST by specific value
template <typename T>
inline ASTPtr MakeAST(T &&value, const Logger &log) {
  using Ty = std::decay_t<T>;
  ASTPtr ast;
  if constexpr (std::is_same_v<T, std::uint64_t>) {
    ast = std::make_unique<IntAST>(value);
  }
  else if constexpr (std::is_same_v<T, double>) {
    ast = std::make_unique<FloatAST>(value);
  }
  else if constexpr (std::is_same_v<T, bool>) {
    ast = std::make_unique<BoolAST>(value);
  }
  else {
    static_assert(false);
  }
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

xstl::Guard Analyzer::NewEnv(bool eval_env) {
  if (eval_env) {
    values_ = MakeEvalEnv(values_);
    enum_values_ = MakeEnumEnv(enum_values_);
    return xstl::Guard([this] {
      values_ = values_->outer();
      enum_values_ = enum_values_->outer();
    });
  }
  else {
    symbols_ = MakeEnv(symbols_);
    user_types_ = MakeEnv(user_types_);
    return xstl::Guard([this] {
      symbols_ = symbols_->outer();
      user_types_ = user_types_->outer();
    });
  }
}

TypePtr Analyzer::AnalyzeOn(PropertyAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(VarLetDefAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(FunDefAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(DeclareAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(TypeAliasAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(StructAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(EnumAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(ImportAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(VarElemAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(LetElemAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(ArgElemAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(EnumElemAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(BlockAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(IfAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(WhenAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(WhileAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(ForInAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(AsmAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(ControlAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(WhenElemAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(BinaryAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(CastAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(UnaryAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(IndexAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(FunCallAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(IntAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(FloatAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(CharAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(IdAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(StringAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(BoolAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(NullAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(ValInitAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(PrimTypeAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(UserTypeAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(FuncTypeAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(VolaTypeAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(ArrayTypeAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(PointerTypeAST &ast) {
  //
}

TypePtr Analyzer::AnalyzeOn(RefTypeAST &ast) {
  //
}

std::optional<EvalNum> Analyzer::EvalOn(PropertyAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(VarLetDefAST &ast) {
  for (const auto &i : ast.defs()) i->Eval(*this);
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(FunDefAST &ast) {
  if (!in_import_) ast.body()->Eval(*this);
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(DeclareAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(TypeAliasAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(StructAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(EnumAST &ast) {
  last_enum_name_ = ast.id();
  last_enum_val_ = 0;
  for (const auto &i : ast.defs()) {
    i->Eval(*this);
  }
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(ImportAST &ast) {
  ++in_import_;
  for (const auto &i : ast.defs()) i->Eval(*this);
  --in_import_;
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(VarElemAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(LetElemAST &ast) {
  // evaluate initial value
  auto val = ast.init()->Eval(*this);
  if (!val) return {};
  // add to environment
  values_->AddItem(ast.id(), val);
  // update AST
  ast.set_init(MakeAST(*val, ast.logger()));
  return val;
}

std::optional<EvalNum> Analyzer::EvalOn(ArgElemAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(EnumElemAST &ast) {
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

std::optional<EvalNum> Analyzer::EvalOn(BlockAST &ast) {
  auto env = NewEnv(true);
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

std::optional<EvalNum> Analyzer::EvalOn(IfAST &ast) {
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

std::optional<EvalNum> Analyzer::EvalOn(WhenAST &ast) {
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

std::optional<EvalNum> Analyzer::EvalOn(WhileAST &ast) {
  // evaluate condition
  auto cond = ast.cond()->Eval(*this);
  if (cond) ast.set_cond(MakeAST(*cond, ast.cond()->logger()));
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()->logger()));
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(ForInAST &ast) {
  // evaluate expression
  auto expr = ast.expr()->Eval(*this);
  if (expr) ast.set_expr(MakeAST(*expr, ast.expr()->logger()));
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()->logger()));
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(AsmAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(ControlAST &ast) {
  // evaluate expression
  if (ast.expr()) {
    auto expr = ast.expr()->Eval(*this);
    if (expr) ast.set_expr(MakeAST(*expr, ast.expr()->logger()));
  }
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(WhenElemAST &ast) {
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

std::optional<EvalNum> Analyzer::EvalOn(BinaryAST &ast) {
  // evaluate rhs
  last_id_ = {};
  auto rhs = ast.rhs()->Eval(*this);
  if (ast.op() != Operator::Access && rhs) {
    ast.set_rhs(MakeAST(*rhs, ast.rhs()->logger()));
  }
  // handle by operator
  if (ast.op() == Operator::Access) {
    // try to get id value of lhs
    auto rhs_id = last_id_;
    last_id_ = {};
    auto lhs = ast.lhs()->Eval(*this);
    // check if is enumerate
    if (last_id_ && rhs_id) {
      auto ev = enum_values_->GetItem(std::string(*last_id_));
      if (ev) {
        auto it = ev->find(std::string(*rhs_id));
        if (it != ev->end()) return it->second;
      }
    }
    return {};
  }
  else if (IsOperatorAssign(ast.op())) {
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
          case Operator::Mod: DO_INT_CALC(%);
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

std::optional<EvalNum> Analyzer::EvalOn(CastAST &ast) {
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

std::optional<EvalNum> Analyzer::EvalOn(UnaryAST &ast) {
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

std::optional<EvalNum> Analyzer::EvalOn(IndexAST &ast) {
  // evaluate expression
  ast.expr()->Eval(*this);
  // evaluate & update index
  auto val = ast.index()->Eval(*this);
  if (val) ast.set_index(MakeAST(*val, ast.index()->logger()));
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(FunCallAST &ast) {
  // evaluate expression
  ast.expr()->Eval(*this);
  // evaluate & update arguments
  for (int i = 0; i < ast.args().size(); ++i) {
    auto val = ast.args()[i]->Eval(*this);
    if (val) ast.set_arg(i, MakeAST(*val, ast.args()[i]->logger()));
  }
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(IntAST &ast) {
  return ast.value();
}

std::optional<EvalNum> Analyzer::EvalOn(FloatAST &ast) {
  return ast.value();
}

std::optional<EvalNum> Analyzer::EvalOn(CharAST &ast) {
  return static_cast<std::uint64_t>(ast.c());
}

std::optional<EvalNum> Analyzer::EvalOn(IdAST &ast) {
  last_id_ = ast.id();
  return values_->GetItem(ast.id());
}

std::optional<EvalNum> Analyzer::EvalOn(StringAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(BoolAST &ast) {
  return static_cast<std::uint64_t>(ast.value());
}

std::optional<EvalNum> Analyzer::EvalOn(NullAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(ValInitAST &ast) {
  // evaluate elements
  for (int i = 0; i < ast.elems().size(); ++i) {
    auto val = ast.elems()[i]->Eval(*this);
    if (val) {
      ast.set_elem(i, MakeAST(*val, ast.elems()[i]->logger()));
    }
  }
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(PrimTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(UserTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(FuncTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(VolaTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(ArrayTypeAST &ast) {
  // evaluate expression
  auto expr = ast.expr()->Eval(*this);
  if (expr) ast.set_expr(MakeAST(*expr, ast.expr()->logger()));
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(PointerTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Analyzer::EvalOn(RefTypeAST &ast) {
  return {};
}
