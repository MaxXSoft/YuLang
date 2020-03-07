#ifndef YULANG_BACK_LLVM_BUILDER_H_
#define YULANG_BACK_LLVM_BUILDER_H_

#include "back/irbuilder.h"

namespace yulang::back {

class LLVMBuilder : public IRBuilderInterface {
 public:
  LLVMBuilder() {}

  IRPtr GenerateOn(define::PropertyAST &ast) override;
  IRPtr GenerateOn(define::VarLetDefAST &ast) override;
  IRPtr GenerateOn(define::FunDefAST &ast) override;
  IRPtr GenerateOn(define::DeclareAST &ast) override;
  IRPtr GenerateOn(define::TypeAliasAST &ast) override;
  IRPtr GenerateOn(define::StructAST &ast) override;
  IRPtr GenerateOn(define::EnumAST &ast) override;
  IRPtr GenerateOn(define::ImportAST &ast) override;
  IRPtr GenerateOn(define::VarElemAST &ast) override;
  IRPtr GenerateOn(define::LetElemAST &ast) override;
  IRPtr GenerateOn(define::ArgElemAST &ast) override;
  IRPtr GenerateOn(define::StructElemAST &ast) override;
  IRPtr GenerateOn(define::EnumElemAST &ast) override;
  IRPtr GenerateOn(define::BlockAST &ast) override;
  IRPtr GenerateOn(define::IfAST &ast) override;
  IRPtr GenerateOn(define::WhenAST &ast) override;
  IRPtr GenerateOn(define::WhileAST &ast) override;
  IRPtr GenerateOn(define::ForInAST &ast) override;
  IRPtr GenerateOn(define::AsmAST &ast) override;
  IRPtr GenerateOn(define::ControlAST &ast) override;
  IRPtr GenerateOn(define::WhenElemAST &ast) override;
  IRPtr GenerateOn(define::BinaryAST &ast) override;
  IRPtr GenerateOn(define::AccessAST &ast) override;
  IRPtr GenerateOn(define::CastAST &ast) override;
  IRPtr GenerateOn(define::UnaryAST &ast) override;
  IRPtr GenerateOn(define::IndexAST &ast) override;
  IRPtr GenerateOn(define::FunCallAST &ast) override;
  IRPtr GenerateOn(define::IntAST &ast) override;
  IRPtr GenerateOn(define::FloatAST &ast) override;
  IRPtr GenerateOn(define::CharAST &ast) override;
  IRPtr GenerateOn(define::IdAST &ast) override;
  IRPtr GenerateOn(define::StringAST &ast) override;
  IRPtr GenerateOn(define::BoolAST &ast) override;
  IRPtr GenerateOn(define::NullAST &ast) override;
  IRPtr GenerateOn(define::ValInitAST &ast) override;
  IRPtr GenerateOn(define::PrimTypeAST &ast) override;
  IRPtr GenerateOn(define::UserTypeAST &ast) override;
  IRPtr GenerateOn(define::FuncTypeAST &ast) override;
  IRPtr GenerateOn(define::VolaTypeAST &ast) override;
  IRPtr GenerateOn(define::ArrayTypeAST &ast) override;
  IRPtr GenerateOn(define::PointerTypeAST &ast) override;
  IRPtr GenerateOn(define::RefTypeAST &ast) override;

  void Dump(std::ostream &os) override;

 private:
  //
};

}  // namespace yulang::back

#endif  // YULANG_BACK_LLVM_BUILDER_H_
