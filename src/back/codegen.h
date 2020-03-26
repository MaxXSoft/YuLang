#ifndef YULANG_BACK_CODEGEN_H_
#define YULANG_BACK_CODEGEN_H_

#include <cstddef>

#include "back/code.h"
#include "define/ast.h"

namespace yulang::back {

// interface of all code generators
class CodeGenInterface {
 public:
  virtual ~CodeGenInterface() = default;

  virtual CodePtr GenerateOn(define::VarLetDefAST &ast) = 0;
  virtual CodePtr GenerateOn(define::FunDefAST &ast) = 0;
  virtual CodePtr GenerateOn(define::DeclareAST &ast) = 0;
  virtual CodePtr GenerateOn(define::TypeAliasAST &ast) = 0;
  virtual CodePtr GenerateOn(define::StructAST &ast) = 0;
  virtual CodePtr GenerateOn(define::EnumAST &ast) = 0;
  virtual CodePtr GenerateOn(define::ImportAST &ast) = 0;
  virtual CodePtr GenerateOn(define::VarLetElemAST &ast) = 0;
  virtual CodePtr GenerateOn(define::ArgElemAST &ast) = 0;
  virtual CodePtr GenerateOn(define::StructElemAST &ast) = 0;
  virtual CodePtr GenerateOn(define::EnumElemAST &ast) = 0;
  virtual CodePtr GenerateOn(define::BlockAST &ast) = 0;
  virtual CodePtr GenerateOn(define::IfAST &ast) = 0;
  virtual CodePtr GenerateOn(define::WhenAST &ast) = 0;
  virtual CodePtr GenerateOn(define::WhileAST &ast) = 0;
  virtual CodePtr GenerateOn(define::ForInAST &ast) = 0;
  virtual CodePtr GenerateOn(define::AsmAST &ast) = 0;
  virtual CodePtr GenerateOn(define::ControlAST &ast) = 0;
  virtual CodePtr GenerateOn(define::WhenElemAST &ast) = 0;
  virtual CodePtr GenerateOn(define::BinaryAST &ast) = 0;
  virtual CodePtr GenerateOn(define::AccessAST &ast) = 0;
  virtual CodePtr GenerateOn(define::CastAST &ast) = 0;
  virtual CodePtr GenerateOn(define::UnaryAST &ast) = 0;
  virtual CodePtr GenerateOn(define::IndexAST &ast) = 0;
  virtual CodePtr GenerateOn(define::FunCallAST &ast) = 0;
  virtual CodePtr GenerateOn(define::IntAST &ast) = 0;
  virtual CodePtr GenerateOn(define::FloatAST &ast) = 0;
  virtual CodePtr GenerateOn(define::CharAST &ast) = 0;
  virtual CodePtr GenerateOn(define::IdAST &ast) = 0;
  virtual CodePtr GenerateOn(define::StringAST &ast) = 0;
  virtual CodePtr GenerateOn(define::BoolAST &ast) = 0;
  virtual CodePtr GenerateOn(define::NullAST &ast) = 0;
  virtual CodePtr GenerateOn(define::ValInitAST &ast) = 0;
  virtual CodePtr GenerateOn(define::PrimTypeAST &ast) = 0;
  virtual CodePtr GenerateOn(define::UserTypeAST &ast) = 0;
  virtual CodePtr GenerateOn(define::FuncTypeAST &ast) = 0;
  virtual CodePtr GenerateOn(define::VolaTypeAST &ast) = 0;
  virtual CodePtr GenerateOn(define::ArrayTypeAST &ast) = 0;
  virtual CodePtr GenerateOn(define::PointerTypeAST &ast) = 0;
  virtual CodePtr GenerateOn(define::RefTypeAST &ast) = 0;

  // get the size of pointer
  virtual std::size_t GetPointerSize() const = 0;
  // dump IRs in current builder
  virtual void Dump(std::ostream &os) = 0;
};

}  // namespace yulang::back

#endif  // YULANG_BACK_CODEGEN_H_
