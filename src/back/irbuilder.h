#ifndef YULANG_BACK_IRBUILDER_H_
#define YULANG_BACK_IRBUILDER_H_

#include <cstddef>

#include "back/ir.h"
#include "define/ast.h"

namespace yulang::back {

// interface of all IR builders
class IRBuilderInterface {
 public:
  virtual ~IRBuilderInterface() = default;

  virtual IRPtr GenerateOn(define::VarLetDefAST &ast) = 0;
  virtual IRPtr GenerateOn(define::FunDefAST &ast) = 0;
  virtual IRPtr GenerateOn(define::DeclareAST &ast) = 0;
  virtual IRPtr GenerateOn(define::TypeAliasAST &ast) = 0;
  virtual IRPtr GenerateOn(define::StructAST &ast) = 0;
  virtual IRPtr GenerateOn(define::EnumAST &ast) = 0;
  virtual IRPtr GenerateOn(define::ImportAST &ast) = 0;
  virtual IRPtr GenerateOn(define::VarLetElemAST &ast) = 0;
  virtual IRPtr GenerateOn(define::ArgElemAST &ast) = 0;
  virtual IRPtr GenerateOn(define::StructElemAST &ast) = 0;
  virtual IRPtr GenerateOn(define::EnumElemAST &ast) = 0;
  virtual IRPtr GenerateOn(define::BlockAST &ast) = 0;
  virtual IRPtr GenerateOn(define::IfAST &ast) = 0;
  virtual IRPtr GenerateOn(define::WhenAST &ast) = 0;
  virtual IRPtr GenerateOn(define::WhileAST &ast) = 0;
  virtual IRPtr GenerateOn(define::ForInAST &ast) = 0;
  virtual IRPtr GenerateOn(define::AsmAST &ast) = 0;
  virtual IRPtr GenerateOn(define::ControlAST &ast) = 0;
  virtual IRPtr GenerateOn(define::WhenElemAST &ast) = 0;
  virtual IRPtr GenerateOn(define::BinaryAST &ast) = 0;
  virtual IRPtr GenerateOn(define::AccessAST &ast) = 0;
  virtual IRPtr GenerateOn(define::CastAST &ast) = 0;
  virtual IRPtr GenerateOn(define::UnaryAST &ast) = 0;
  virtual IRPtr GenerateOn(define::IndexAST &ast) = 0;
  virtual IRPtr GenerateOn(define::FunCallAST &ast) = 0;
  virtual IRPtr GenerateOn(define::IntAST &ast) = 0;
  virtual IRPtr GenerateOn(define::FloatAST &ast) = 0;
  virtual IRPtr GenerateOn(define::CharAST &ast) = 0;
  virtual IRPtr GenerateOn(define::IdAST &ast) = 0;
  virtual IRPtr GenerateOn(define::StringAST &ast) = 0;
  virtual IRPtr GenerateOn(define::BoolAST &ast) = 0;
  virtual IRPtr GenerateOn(define::NullAST &ast) = 0;
  virtual IRPtr GenerateOn(define::ValInitAST &ast) = 0;
  virtual IRPtr GenerateOn(define::PrimTypeAST &ast) = 0;
  virtual IRPtr GenerateOn(define::UserTypeAST &ast) = 0;
  virtual IRPtr GenerateOn(define::FuncTypeAST &ast) = 0;
  virtual IRPtr GenerateOn(define::VolaTypeAST &ast) = 0;
  virtual IRPtr GenerateOn(define::ArrayTypeAST &ast) = 0;
  virtual IRPtr GenerateOn(define::PointerTypeAST &ast) = 0;
  virtual IRPtr GenerateOn(define::RefTypeAST &ast) = 0;

  // get the size of pointer
  virtual std::size_t GetPointerSize() const = 0;
  // dump IRs in current builder
  virtual void Dump(std::ostream &os) = 0;
};

}  // namespace yulang::back

#endif  // YULANG_BACK_IRBUILDER_H_
