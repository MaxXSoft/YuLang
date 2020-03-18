#ifndef YULANG_BACK_LLVM_BUILDER_H_
#define YULANG_BACK_LLVM_BUILDER_H_

#include <memory>
#include <string>
#include <stack>
#include <utility>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/BasicBlock.h"

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
  // pair for storing target block of break & continue
  using BreakCont = std::pair<llvm::BasicBlock *, llvm::BasicBlock *>;

  // perform initialization
  void Init();
  // switch to a new environment
  xstl::Guard NewEnv();
  // automatically create load when specific value is a left value
  llvm::Value *UseValue(llvm::Value *val, const define::TypePtr &type);
  // automatically create load, using specific AST pointer
  llvm::Value *UseValue(const define::ASTPtr &ast);

  // create new value in target 'size_t' type
  llvm::Value *CreateSizeValue(std::size_t val);
  // create new value in target 'size_t' type by specific value
  llvm::Value *CreateSizeValue(llvm::Value *val,
                               const define::TypePtr &type);
  // create new allocation in current function
  llvm::AllocaInst *CreateAlloca(const define::TypePtr &type);
  // create new load instruction
  llvm::LoadInst *CreateLoad(llvm::Value *val,
                             const define::TypePtr &type);
  // create new store instruction
  // if 'dst' is a structure or an array, generate 'memcpy'
  void CreateStore(llvm::Value *val, llvm::Value *dst,
                   const define::TypePtr &type);
  // create new variable/constant definition
  void CreateVarLet(const std::string &id, const define::TypePtr &type,
                    const define::ASTPtr &init);
  // create new function call
  llvm::CallInst *CreateCall(llvm::Value *callee,
                             llvm::ArrayRef<llvm::Value *> args,
                             const define::TypePtrList &args_type);
  // create binary operations
  llvm::Value *CreateBinOp(define::Operator op, llvm::Value *lhs,
                           llvm::Value *rhs, const define::TypePtr &lhs_ty,
                           const define::TypePtr &rhs_ty);

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
  // table of values & types
  xstl::NestedMapPtr<std::string, llvm::Value *> vals_;
  xstl::NestedMapPtr<std::string, llvm::Type *> types_;
  // global constructor & destructor
  llvm::Function *ctor_, *dtor_;
  // used when generating properties
  llvm::GlobalVariable::LinkageTypes link_;
  // used when generating function definitions
  llvm::Value *ret_val_;
  llvm::BasicBlock *func_exit_;
  // used when generating when statements
  llvm::BasicBlock *when_end_;
  llvm::Value *when_expr_;
  // used when generating loops
  std::stack<BreakCont> break_cont_;
};

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_BUILDER_H_
