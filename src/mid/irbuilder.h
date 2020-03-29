#ifndef YULANG_MID_IRBUILDER_H_
#define YULANG_MID_IRBUILDER_H_

#include <stack>
#include <utility>

#include "define/ast.h"
#include "mid/usedef.h"
#include "mid/module.h"
#include "xstl/guard.h"
#include "xstl/nested.h"

namespace yulang::mid {

class IRBuilder {
 public:
  IRBuilder() : vals_(xstl::MakeNestedMap<std::string, SSAPtr>()) {}

  SSAPtr GenerateOn(define::VarLetDefAST &ast);
  SSAPtr GenerateOn(define::FunDefAST &ast);
  SSAPtr GenerateOn(define::DeclareAST &ast);
  SSAPtr GenerateOn(define::TypeAliasAST &ast);
  SSAPtr GenerateOn(define::StructAST &ast);
  SSAPtr GenerateOn(define::EnumAST &ast);
  SSAPtr GenerateOn(define::ImportAST &ast);
  SSAPtr GenerateOn(define::VarLetElemAST &ast);
  SSAPtr GenerateOn(define::ArgElemAST &ast);
  SSAPtr GenerateOn(define::StructElemAST &ast);
  SSAPtr GenerateOn(define::EnumElemAST &ast);
  SSAPtr GenerateOn(define::BlockAST &ast);
  SSAPtr GenerateOn(define::IfAST &ast);
  SSAPtr GenerateOn(define::WhenAST &ast);
  SSAPtr GenerateOn(define::WhileAST &ast);
  SSAPtr GenerateOn(define::ForInAST &ast);
  SSAPtr GenerateOn(define::AsmAST &ast);
  SSAPtr GenerateOn(define::ControlAST &ast);
  SSAPtr GenerateOn(define::WhenElemAST &ast);
  SSAPtr GenerateOn(define::BinaryAST &ast);
  SSAPtr GenerateOn(define::AccessAST &ast);
  SSAPtr GenerateOn(define::CastAST &ast);
  SSAPtr GenerateOn(define::UnaryAST &ast);
  SSAPtr GenerateOn(define::IndexAST &ast);
  SSAPtr GenerateOn(define::FunCallAST &ast);
  SSAPtr GenerateOn(define::IntAST &ast);
  SSAPtr GenerateOn(define::FloatAST &ast);
  SSAPtr GenerateOn(define::CharAST &ast);
  SSAPtr GenerateOn(define::IdAST &ast);
  SSAPtr GenerateOn(define::StringAST &ast);
  SSAPtr GenerateOn(define::BoolAST &ast);
  SSAPtr GenerateOn(define::NullAST &ast);
  SSAPtr GenerateOn(define::ValInitAST &ast);
  SSAPtr GenerateOn(define::PrimTypeAST &ast);
  SSAPtr GenerateOn(define::UserTypeAST &ast);
  SSAPtr GenerateOn(define::FuncTypeAST &ast);
  SSAPtr GenerateOn(define::VolaTypeAST &ast);
  SSAPtr GenerateOn(define::ArrayTypeAST &ast);
  SSAPtr GenerateOn(define::PointerTypeAST &ast);
  SSAPtr GenerateOn(define::RefTypeAST &ast);

  // getters
  const Module &module() const { return module_; }

 private:
  // pair for storing target block of break & continue
  using BreakCont = std::pair<BlockPtr, BlockPtr>;

  // switch to a new environment
  xstl::Guard NewEnv();
  // create binary operation
  SSAPtr CreateBinOp(define::Operator op, const SSAPtr &lhs,
                     const SSAPtr &rhs);

  // module for storing IRs
  Module module_;
  // table of values
  xstl::NestedMapPtr<std::string, SSAPtr> vals_;
  // used when generating var/let definitions
  define::Property last_prop_;
  // used when generating function definitions
  SSAPtr ret_val_;
  BlockPtr func_exit_;
  // used when generating when statement
  BlockPtr when_end_;
  SSAPtr when_expr_;
  // used when generating loops
  std::stack<BreakCont> break_cont_;
};

}  // namespace yulang::mid

#endif  // YULANG_MID_IRBUILDER_H_
