#ifndef YULANG_MID_IRBUILDER_H_
#define YULANG_MID_IRBUILDER_H_

#include "define/ast.h"
#include "mid/usedef.h"

namespace yulang::mid {

class IRBuilder {
 public:
  IRBuilder() {}

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

 private:
  //
};

}  // namespace yulang::mid

#endif  // YULANG_MID_IRBUILDER_H_
