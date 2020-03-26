#ifndef YULANG_BACK_LLVM_GENERATOR_H_
#define YULANG_BACK_LLVM_GENERATOR_H_

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
#include "back/codegen.h"
#include "back/llvm/ir.h"
#include "xstl/guard.h"
#include "xstl/nested.h"

namespace yulang::back::ll {

class LLVMGen : public CodeGenInterface {
 public:
  LLVMGen(const std::string &file_name)
      : builder_(context_),
        module_(std::make_unique<llvm::Module>(file_name, context_)) {
    Init();
  }

  CodePtr GenerateOn(define::VarLetDefAST &ast) override;
  CodePtr GenerateOn(define::FunDefAST &ast) override;
  CodePtr GenerateOn(define::DeclareAST &ast) override;
  CodePtr GenerateOn(define::TypeAliasAST &ast) override;
  CodePtr GenerateOn(define::StructAST &ast) override;
  CodePtr GenerateOn(define::EnumAST &ast) override;
  CodePtr GenerateOn(define::ImportAST &ast) override;
  CodePtr GenerateOn(define::VarLetElemAST &ast) override;
  CodePtr GenerateOn(define::ArgElemAST &ast) override;
  CodePtr GenerateOn(define::StructElemAST &ast) override;
  CodePtr GenerateOn(define::EnumElemAST &ast) override;
  CodePtr GenerateOn(define::BlockAST &ast) override;
  CodePtr GenerateOn(define::IfAST &ast) override;
  CodePtr GenerateOn(define::WhenAST &ast) override;
  CodePtr GenerateOn(define::WhileAST &ast) override;
  CodePtr GenerateOn(define::ForInAST &ast) override;
  CodePtr GenerateOn(define::AsmAST &ast) override;
  CodePtr GenerateOn(define::ControlAST &ast) override;
  CodePtr GenerateOn(define::WhenElemAST &ast) override;
  CodePtr GenerateOn(define::BinaryAST &ast) override;
  CodePtr GenerateOn(define::AccessAST &ast) override;
  CodePtr GenerateOn(define::CastAST &ast) override;
  CodePtr GenerateOn(define::UnaryAST &ast) override;
  CodePtr GenerateOn(define::IndexAST &ast) override;
  CodePtr GenerateOn(define::FunCallAST &ast) override;
  CodePtr GenerateOn(define::IntAST &ast) override;
  CodePtr GenerateOn(define::FloatAST &ast) override;
  CodePtr GenerateOn(define::CharAST &ast) override;
  CodePtr GenerateOn(define::IdAST &ast) override;
  CodePtr GenerateOn(define::StringAST &ast) override;
  CodePtr GenerateOn(define::BoolAST &ast) override;
  CodePtr GenerateOn(define::NullAST &ast) override;
  CodePtr GenerateOn(define::ValInitAST &ast) override;
  CodePtr GenerateOn(define::PrimTypeAST &ast) override;
  CodePtr GenerateOn(define::UserTypeAST &ast) override;
  CodePtr GenerateOn(define::FuncTypeAST &ast) override;
  CodePtr GenerateOn(define::VolaTypeAST &ast) override;
  CodePtr GenerateOn(define::ArrayTypeAST &ast) override;
  CodePtr GenerateOn(define::PointerTypeAST &ast) override;
  CodePtr GenerateOn(define::RefTypeAST &ast) override;

  std::size_t GetPointerSize() const override;
  void Dump(std::ostream &os) override;

 private:
  // pair for storing target block of break & continue
  using BreakCont = std::pair<llvm::BasicBlock *, llvm::BasicBlock *>;

  // perform initialization
  void Init();
  // switch to a new environment
  xstl::Guard NewEnv();
  // switch to body of constructor/destructor
  xstl::Guard EnterGlobalFunc(bool is_dtor);
  // get linkage type
  llvm::GlobalValue::LinkageTypes GetLinkType(define::Property prop);
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
  // create new function call
  llvm::Value *CreateCall(llvm::Value *callee,
                          llvm::ArrayRef<llvm::Value *> args,
                          const define::TypePtr &ret);
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
  // check if type is a raw struct (not a reference of struct)
  bool IsTypeRawStruct(const define::TypePtr &type);

  // LLVM stuffs
  llvm::LLVMContext context_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::Module> module_;
  // table of values & types
  xstl::NestedMapPtr<std::string, llvm::Value *> vals_;
  xstl::NestedMapPtr<std::string, llvm::Type *> types_;
  // global constructor & destructor
  llvm::Function *ctor_, *dtor_;
  // used when generating function definitions
  llvm::Value *ret_val_;
  llvm::BasicBlock *func_exit_;
  // used when generating var/let definitions
  define::Property last_prop_;
  // used when generating when statements
  llvm::BasicBlock *when_end_;
  llvm::Value *when_expr_;
  // used when generating loops
  std::stack<BreakCont> break_cont_;
};

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_GENERATOR_H_
