#include "front/eval.h"

#include <type_traits>
#include <cmath>
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

// create a new AST by 'EvalNum'
inline ASTPtr MakeAST(const EvalNum &num, const ASTPtr &ast) {
  const auto &type = ast->ast_type();
  ASTPtr ret;
  // handle by type of 'ast'
  if (type->IsInteger() || type->IsEnum()) {
    // generate int AST
    auto val = std::get_if<std::uint64_t>(&num);
    assert(val);
    ret = std::make_unique<IntAST>(*val);
  }
  else if (type->IsFloat()) {
    // generate float AST
    // NOTE: there is no loss of precision due to floating point promotion
    //  ref: section 4.6 from n3337
    if (type->GetSize() == 4) {
      auto val = std::get_if<float>(&num);
      assert(val);
      ret = std::make_unique<FloatAST>(*val);
    }
    else {
      auto val = std::get_if<double>(&num);
      assert(val);
      ret = std::make_unique<FloatAST>(*val);
    }
  }
  else if (type->IsBool()) {
    // generate bool AST
    auto val = std::get_if<std::uint64_t>(&num);
    assert(val);
    ret = std::make_unique<BoolAST>(*val);
  }
  else if (type->IsNull()) {
    // generate null AST
    ret = std::make_unique<NullAST>();
  }
  else {
    assert(false);
  }
  // set additional information
  ret->set_logger(ast->logger());
  if (type->IsRightValue()) {
    ret->set_ast_type(type);
  }
  else {
    ret->set_ast_type(type->GetValueType(true));
  }
  return ret;
}

// cast 'EvalNum' to boolean
inline bool CastToBool(const EvalNum &num) {
  return std::visit([](auto &&args) { return args != 0; }, num);
}

// cast 'EvalNum' to specific type
inline EvalNum CastToType(const EvalNum &num, const TypePtr &type) {
  assert(type->IsInteger() || type->IsBool() || type->IsFloat());
  return std::visit([&type](auto &&arg) -> EvalNum {
    if (type->IsBool()) {
      return static_cast<std::uint64_t>(!!arg);
    }
    else if (type->GetSize() == 1) {
      auto ans = type->IsUnsigned() ? static_cast<std::uint8_t>(arg)
                                    : static_cast<std::int8_t>(arg);
      return static_cast<std::uint64_t>(ans);
    }
    else if (type->GetSize() == 2) {
      auto ans = type->IsUnsigned() ? static_cast<std::uint16_t>(arg)
                                    : static_cast<std::int16_t>(arg);
      return static_cast<std::uint64_t>(ans);
    }
    else if (type->GetSize() == 4) {
      if (type->IsInteger()) {
        auto ans = type->IsUnsigned() ? static_cast<std::uint32_t>(arg)
                                      : static_cast<std::int32_t>(arg);
        return static_cast<std::uint64_t>(ans);
      }
      else {
        return static_cast<float>(arg);
      }
    }
    else if (type->GetSize() == 8) {
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
      return static_cast<std::uint64_t>(0);
    }
  }, num);
}

// cast value to specific type
inline std::uint64_t CastToType(std::uint64_t num, const TypePtr &type) {
  assert(type->IsInteger());
  if (type->GetSize() == 1) {
    return type->IsUnsigned() ? static_cast<std::uint8_t>(num)
                              : static_cast<std::int8_t>(num);
  }
  else if (type->GetSize() == 2) {
    return type->IsUnsigned() ? static_cast<std::uint16_t>(num)
                              : static_cast<std::int16_t>(num);
  }
  else if (type->GetSize() == 4) {
    return type->IsUnsigned() ? static_cast<std::uint32_t>(num)
                              : static_cast<std::int32_t>(num);
  }
  else if (type->GetSize() == 8) {
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
  // do nothing, since imported ASTs are evaluated
  // in Analyzer::AnalyzeOn(ImportAST &)
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(VarLetElemAST &ast) {
  // do not evaluate reference
  if (ast.ast_type()->IsReference()) return {};
  // evaluate initial value
  if (!ast.init()) return {};
  auto val = ast.init()->Eval(*this);
  if (!val) return {};
  // add to environment
  if (!ast.is_var()) values_->AddItem(ast.id(), val);
  // update AST
  ast.set_init(MakeAST(*val, ast.init()));
  return {};
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
    ast.set_expr(MakeAST(*num, ast.expr()));
  }
  last_enum_val_ = CastToType(last_enum_val_, ast.ast_type());
  // add to environment
  if (enum_values_->GetItem(last_enum_name_, false)) {
    auto &val = enum_values_->AccessItem(last_enum_name_);
    val->insert({ast.id(), last_enum_val_++});
  }
  else {
    enum_values_->AddItem(last_enum_name_,
                          {{{ast.id(), last_enum_val_++}}});
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(BlockAST &ast) {
  auto env = NewEnv();
  bool valid = true;
  for (std::size_t i = 0; i < ast.stmts().size(); ++i) {
    // evaluate current statement
    auto val = ast.stmts()[i]->Eval(*this);
    if (val) {
      // update current statement
      ast.set_stmt(i, MakeAST(*val, ast.stmts()[i]));
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
  auto else_val = ast.else_then() ? ast.else_then()->Eval(*this)
                                  : std::nullopt;
  if (else_val) {
    ast.set_else_then(MakeAST(*else_val, ast.else_then()));
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
    ast.set_expr(MakeAST(*last_when_expr_, ast.expr()));
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
    ast.set_else_then(MakeAST(*else_val, ast.else_then()));
  }
  // return expression's value
  if (!last_when_expr_) return {};
  return ret ? ret : else_val;
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
  // evaluate expression
  auto expr = ast.expr()->Eval(*this);
  if (expr) ast.set_expr(MakeAST(*expr, ast.expr()));
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(AsmAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(ControlAST &ast) {
  // evaluate expression
  if (ast.expr()) {
    auto expr = ast.expr()->Eval(*this);
    if (expr) ast.set_expr(MakeAST(*expr, ast.expr()));
  }
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(WhenElemAST &ast) {
  bool valid = false;
  // evaluate conditions
  for (std::size_t i = 0; i < ast.conds().size(); ++i) {
    auto val = ast.conds()[i]->Eval(*this);
    if (val) {
      // update condition
      ast.set_cond(i, MakeAST(*val, ast.conds()[i]));
      // check if valid
      if (last_when_expr_ && CheckEqual(*val, *last_when_expr_)) {
        valid = true;
      }
    }
  }
  // evaluate body
  auto body = ast.body()->Eval(*this);
  if (body) ast.set_body(MakeAST(*body, ast.body()));
  return valid ? body : std::nullopt;
}

std::optional<EvalNum> Evaluator::EvalOn(BinaryAST &ast) {
  // evaluate rhs
  auto rhs = ast.rhs()->Eval(*this);
  if (rhs) ast.set_rhs(MakeAST(*rhs, ast.rhs()));
  // handle by operator
  if (IsOperatorAssign(ast.op())) {
    // do not evaluate rhs, just return null
    return {};
  }
  else {
    // evaluate & update lhs
    auto lhs = ast.lhs()->Eval(*this);
    if (lhs) ast.set_lhs(MakeAST(*lhs, ast.lhs()));
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
  // check if is enumerate
  auto type = ast.expr()->ast_type();
  if (type->IsEnum()) {
    auto ev = enum_values_->GetItem(type->GetTypeId());
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
  ast.set_expr(MakeAST(*val, ast.expr()));
  // perform type casting
  if (!ast.ast_type()->IsInteger() && !ast.ast_type()->IsBool() &&
      !ast.ast_type()->IsFloat()) {
    return {};
  }
  return CastToType(*val, ast.ast_type());
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
  return values_->GetItem(ast.id());
}

std::optional<EvalNum> Evaluator::EvalOn(StringAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(BoolAST &ast) {
  return static_cast<std::uint64_t>(ast.value());
}

std::optional<EvalNum> Evaluator::EvalOn(NullAST &ast) {
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
  if (expr) ast.set_expr(MakeAST(*expr, ast.expr()));
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(PointerTypeAST &ast) {
  return {};
}

std::optional<EvalNum> Evaluator::EvalOn(RefTypeAST &ast) {
  return {};
}
