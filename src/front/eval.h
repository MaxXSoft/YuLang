#ifndef YULANG_FRONT_EVAL_H_
#define YULANG_FRONT_EVAL_H_

#include <optional>
#include <string>
#include <cstdint>

#include "define/ast.h"
#include "define/symbol.h"

#include "xstl/guard.h"

namespace yulang::front {

class Evaluator {
 public:
  Evaluator() { Reset(); }

  void Reset() {
    values_ = define::MakeEvalEnv();
    enum_values_ = define::MakeEnumEnv();
  }

  std::optional<define::EvalNum> EvalOn(define::PropertyAST &ast);
  std::optional<define::EvalNum> EvalOn(define::VarLetDefAST &ast);
  std::optional<define::EvalNum> EvalOn(define::FunDefAST &ast);
  std::optional<define::EvalNum> EvalOn(define::DeclareAST &ast);
  std::optional<define::EvalNum> EvalOn(define::TypeAliasAST &ast);
  std::optional<define::EvalNum> EvalOn(define::StructAST &ast);
  std::optional<define::EvalNum> EvalOn(define::EnumAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ImportAST &ast);
  std::optional<define::EvalNum> EvalOn(define::VarElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::LetElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ArgElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::StructElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::EnumElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::BlockAST &ast);
  std::optional<define::EvalNum> EvalOn(define::IfAST &ast);
  std::optional<define::EvalNum> EvalOn(define::WhenAST &ast);
  std::optional<define::EvalNum> EvalOn(define::WhileAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ForInAST &ast);
  std::optional<define::EvalNum> EvalOn(define::AsmAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ControlAST &ast);
  std::optional<define::EvalNum> EvalOn(define::WhenElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::BinaryAST &ast);
  std::optional<define::EvalNum> EvalOn(define::AccessAST &ast);
  std::optional<define::EvalNum> EvalOn(define::CastAST &ast);
  std::optional<define::EvalNum> EvalOn(define::UnaryAST &ast);
  std::optional<define::EvalNum> EvalOn(define::IndexAST &ast);
  std::optional<define::EvalNum> EvalOn(define::FunCallAST &ast);
  std::optional<define::EvalNum> EvalOn(define::IntAST &ast);
  std::optional<define::EvalNum> EvalOn(define::FloatAST &ast);
  std::optional<define::EvalNum> EvalOn(define::CharAST &ast);
  std::optional<define::EvalNum> EvalOn(define::IdAST &ast);
  std::optional<define::EvalNum> EvalOn(define::StringAST &ast);
  std::optional<define::EvalNum> EvalOn(define::BoolAST &ast);
  std::optional<define::EvalNum> EvalOn(define::NullAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ValInitAST &ast);
  std::optional<define::EvalNum> EvalOn(define::PrimTypeAST &ast);
  std::optional<define::EvalNum> EvalOn(define::UserTypeAST &ast);
  std::optional<define::EvalNum> EvalOn(define::FuncTypeAST &ast);
  std::optional<define::EvalNum> EvalOn(define::VolaTypeAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ArrayTypeAST &ast);
  std::optional<define::EvalNum> EvalOn(define::PointerTypeAST &ast);
  std::optional<define::EvalNum> EvalOn(define::RefTypeAST &ast);

 private:
  // switch to new environment
  xstl::Guard NewEnv();

  // evaluated values
  define::EvalEnvPtr values_;
  // evaluated enumerations
  define::EnumEnvPtr enum_values_;
  // used when evaluating enumerations
  std::string last_enum_name_;
  std::uint64_t last_enum_val_;
  // used when evaluating 'when' statements
  std::optional<EvalNum> last_when_expr_;
  // used when evaluating identifiers
  std::optional<std::string> last_id_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_EVAL_H_
