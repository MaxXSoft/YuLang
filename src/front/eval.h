#ifndef YULANG_FRONT_EVAL_H_
#define YULANG_FRONT_EVAL_H_

#include <cstdint>
#include <optional>
#include <string>

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

  // Semantic analysis also evaluates initializers and array dimensions.
  xstl::Guard NewEnv();
  void Mask(const std::string &id) { values_->AccessItem(id) = std::nullopt; }

  std::optional<define::EvalNum> EvalOn(define::VarLetDefAST &ast);
  std::optional<define::EvalNum> EvalOn(define::FunDefAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::DeclareAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::TypeAliasAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::StructAST &ast);
  std::optional<define::EvalNum> EvalOn(define::EnumAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::ImportAST &ast);
  std::optional<define::EvalNum> EvalOn(define::VarLetElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ArgElemAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::StructElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::EnumElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::BlockAST &ast);
  std::optional<define::EvalNum> EvalOn(define::IfAST &ast);
  std::optional<define::EvalNum> EvalOn(define::WhenAST &ast);
  std::optional<define::EvalNum> EvalOn(define::WhileAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ForInAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::AsmAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ControlAST &ast);
  std::optional<define::EvalNum> EvalOn(define::WhenElemAST &ast);
  std::optional<define::EvalNum> EvalOn(define::BinaryAST &ast);
  std::optional<define::EvalNum> EvalOn(define::AccessAST &ast);
  std::optional<define::EvalNum> EvalOn(define::CastAST &ast);
  std::optional<define::EvalNum> EvalOn(define::UnaryAST &ast);
  std::optional<define::EvalNum> EvalOn(define::IndexAST &ast);
  std::optional<define::EvalNum> EvalOn(define::FunCallAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::IntAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::FloatAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::CharAST &ast);
  std::optional<define::EvalNum> EvalOn(define::IdAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::StringAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::BoolAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::NullAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ValInitAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::PrimTypeAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::UserTypeAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::FuncTypeAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::VolaTypeAST &ast);
  std::optional<define::EvalNum> EvalOn(define::ArrayTypeAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::PointerTypeAST &ast);
  static std::optional<define::EvalNum> EvalOn(define::RefTypeAST &ast);

 private:
  // evaluated values
  define::EvalEnvPtr values_;
  // evaluated enumerations
  define::EnumEnvPtr enum_values_;
  // used when evaluating enumerations
  std::string last_enum_name_;
  std::uint64_t last_enum_val_{};
  // used when evaluating 'when' statements
  std::optional<define::EvalNum> last_when_expr_;
  // used when evaluating identifiers
  std::optional<std::string> last_id_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_EVAL_H_
