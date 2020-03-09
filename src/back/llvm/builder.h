#ifndef YULANG_BACK_LLVM_BUILDER_H_
#define YULANG_BACK_LLVM_BUILDER_H_

#include <memory>
#include <string>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

#include "define/type.h"
#include "back/irbuilder.h"
#include "back/llvm/ir.h"
#include "xstl/guard.h"
#include "xstl/nested.h"

namespace yulang::back::ll {

class LLVMBuilder : public IRBuilderInterface {
 public:
  LLVMBuilder(const std::string &file_name)
      : builder_(context_),
        module_(std::make_unique<llvm::Module>(file_name, context_)) {
    Init();
  }

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

  std::size_t GetPointerSize() const override;
  void Dump(std::ostream &os) override;

 private:
  // perform initialization
  void Init();
  // switch to a new environment
  xstl::Guard NewEnv();
  // create new allocation in current function
  llvm::AllocaInst *CreateAlloca(llvm::Type *type);
  // create new store instruction
  void CreateStore(llvm::Value *val, llvm::Value *dst,
                   const define::TypePtr &type);

  // generate on 'yulang::define::TypePtr'
  llvm::Type *GenerateType(const define::TypePtr &type);
  llvm::Type *GeneratePrimType(const define::TypePtr &type);
  llvm::Type *GenerateStructType(const define::TypePtr &type);
  llvm::Type *GenerateEnumType(const define::TypePtr &type);
  llvm::Type *GenerateFuncType(const define::TypePtr &type);
  llvm::Type *GenerateArrayType(const define::TypePtr &type);
  llvm::Type *GeneratePointerType(const define::TypePtr &type);
  llvm::Type *GenerateRefType(const define::TypePtr &type);

  // LLVM stuffs
  llvm::LLVMContext context_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::Module> module_;
  // used when generating function definition
  llvm::Value *ret_val_;
  // table of values & types
  xstl::NestedMapPtr<std::string, llvm::Value *> vals_;
  xstl::NestedMapPtr<std::string, llvm::Type *> types_;
  // used when generating properties
  bool is_last_global_;
};

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_BUILDER_H_