#ifndef YULANG_FRONT_ANALYZER_H_
#define YULANG_FRONT_ANALYZER_H_

#include <optional>
#include <string>
#include <string_view>
#include <functional>
#include <cstdint>

#include "define/ast.h"
#include "define/type.h"
#include "define/symbol.h"
#include "define/token.h"
#include "front/eval.h"
#include "front/logger.h"

#include "xstl/guard.h"

namespace yulang::front {

class Analyzer {
 public:
  Analyzer(Evaluator &eval) : eval_(eval) { Reset(); }

  void Reset() {
    symbols_ = define::MakeEnv();
    user_types_ = define::MakeEnv();
    in_loop_ = 0;
  }

  define::TypePtr AnalyzeOn(define::PropertyAST &ast);
  define::TypePtr AnalyzeOn(define::VarLetDefAST &ast);
  define::TypePtr AnalyzeOn(define::FunDefAST &ast);
  define::TypePtr AnalyzeOn(define::DeclareAST &ast);
  define::TypePtr AnalyzeOn(define::TypeAliasAST &ast);
  define::TypePtr AnalyzeOn(define::StructAST &ast);
  define::TypePtr AnalyzeOn(define::EnumAST &ast);
  define::TypePtr AnalyzeOn(define::ImportAST &ast);
  define::TypePtr AnalyzeOn(define::VarElemAST &ast);
  define::TypePtr AnalyzeOn(define::LetElemAST &ast);
  define::TypePtr AnalyzeOn(define::ArgElemAST &ast);
  define::TypePtr AnalyzeOn(define::StructElemAST &ast);
  define::TypePtr AnalyzeOn(define::EnumElemAST &ast);
  define::TypePtr AnalyzeOn(define::BlockAST &ast);
  define::TypePtr AnalyzeOn(define::IfAST &ast);
  define::TypePtr AnalyzeOn(define::WhenAST &ast);
  define::TypePtr AnalyzeOn(define::WhileAST &ast);
  define::TypePtr AnalyzeOn(define::ForInAST &ast);
  define::TypePtr AnalyzeOn(define::AsmAST &ast);
  define::TypePtr AnalyzeOn(define::ControlAST &ast);
  define::TypePtr AnalyzeOn(define::WhenElemAST &ast);
  define::TypePtr AnalyzeOn(define::BinaryAST &ast);
  define::TypePtr AnalyzeOn(define::AccessAST &ast);
  define::TypePtr AnalyzeOn(define::CastAST &ast);
  define::TypePtr AnalyzeOn(define::UnaryAST &ast);
  define::TypePtr AnalyzeOn(define::IndexAST &ast);
  define::TypePtr AnalyzeOn(define::FunCallAST &ast);
  define::TypePtr AnalyzeOn(define::IntAST &ast);
  define::TypePtr AnalyzeOn(define::FloatAST &ast);
  define::TypePtr AnalyzeOn(define::CharAST &ast);
  define::TypePtr AnalyzeOn(define::IdAST &ast);
  define::TypePtr AnalyzeOn(define::StringAST &ast);
  define::TypePtr AnalyzeOn(define::BoolAST &ast);
  define::TypePtr AnalyzeOn(define::NullAST &ast);
  define::TypePtr AnalyzeOn(define::ValInitAST &ast);
  define::TypePtr AnalyzeOn(define::PrimTypeAST &ast);
  define::TypePtr AnalyzeOn(define::UserTypeAST &ast);
  define::TypePtr AnalyzeOn(define::FuncTypeAST &ast);
  define::TypePtr AnalyzeOn(define::VolaTypeAST &ast);
  define::TypePtr AnalyzeOn(define::ArrayTypeAST &ast);
  define::TypePtr AnalyzeOn(define::PointerTypeAST &ast);
  define::TypePtr AnalyzeOn(define::RefTypeAST &ast);

 private:
  using IdSetter = std::function<void(const std::string &)>;

  // switch to new environment
  xstl::Guard NewEnv();
  // enter a new function
  xstl::Guard EnterFunc(const define::TypePtr &ret);
  // perform name mangling
  std::string MangleFuncName(const std::string &id,
                             const define::TypePtrList &args);
  // check and add user type
  bool AddUserType(const Logger &log, const std::string &id,
                   define::TypePtr type);
  // check and add variables/constants
  bool AddVarConst(const Logger &log, const std::string &id,
                   define::TypePtr type, define::TypePtr init,
                   bool is_var);
  // find function type in current environment
  // call 'id_setter' using function name
  // print error message if not found
  define::TypePtr FindFuncType(const Logger &log, const std::string &id,
                               const define::TypePtrList &args,
                               IdSetter id_setter);
  // check if is valid operator overloading
  // if so, return function's return type, otherwise log error
  std::optional<define::TypePtr> CheckOpOverload(
      const Logger &log, const std::string &op_name,
      const define::TypePtrList &args, IdSetter id_setter);

  // evaluator
  Evaluator &eval_;
  // symbol tables & user defined types (structs, enums, aliases)
  define::EnvPtr symbols_, user_types_;
  // used when analyzing properties
  define::PropertyAST::Property last_prop_;
  // used when analyzing functions
  define::TypePtr cur_ret_;
  // used when analyzing structures
  std::string_view last_struct_elem_name_;
  // used when analyzing enumerations
  define::TypePtr last_enum_type_;
  std::string last_enum_elem_name_;
  // used when analyzing 'when' statements
  define::TypePtr last_when_expr_type_;
  // used when analyzing while loop & for loop
  std::uint64_t in_loop_;
  // used when analyzing identifiers
  std::optional<std::string> last_id_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_ANALYZER_H_
