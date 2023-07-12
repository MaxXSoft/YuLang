#ifndef YULANG_BACK_LLVM_GENERATOR_H_
#define YULANG_BACK_LLVM_GENERATOR_H_

#include <memory>
#include <string>
#include <vector>
#include <utility>
#include <unordered_map>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

#include "back/codegen.h"
#include "define/type.h"
#include "back/llvm/define.h"

namespace yulang::back::ll {

class LLVMGen : public CodeGenInterface {
 public:
  LLVMGen(const std::string &file_name)
      : builder_(context_),
        module_(std::make_unique<llvm::Module>(file_name, context_)) {}

  void GenerateOn(mid::LoadSSA &ssa) override;
  void GenerateOn(mid::StoreSSA &ssa) override;
  void GenerateOn(mid::AccessSSA &ssa) override;
  void GenerateOn(mid::BinarySSA &ssa) override;
  void GenerateOn(mid::UnarySSA &ssa) override;
  void GenerateOn(mid::CastSSA &ssa) override;
  void GenerateOn(mid::CallSSA &ssa) override;
  void GenerateOn(mid::BranchSSA &ssa) override;
  void GenerateOn(mid::JumpSSA &ssa) override;
  void GenerateOn(mid::ReturnSSA &ssa) override;
  void GenerateOn(mid::FunctionSSA &ssa) override;
  void GenerateOn(mid::GlobalVarSSA &ssa) override;
  void GenerateOn(mid::AllocaSSA &ssa) override;
  void GenerateOn(mid::BlockSSA &ssa) override;
  void GenerateOn(mid::ArgRefSSA &ssa) override;
  void GenerateOn(mid::AsmSSA &ssa) override;
  void GenerateOn(mid::ConstIntSSA &ssa) override;
  void GenerateOn(mid::ConstFloatSSA &ssa) override;
  void GenerateOn(mid::ConstStrSSA &ssa) override;
  void GenerateOn(mid::ConstStructSSA &ssa) override;
  void GenerateOn(mid::ConstArraySSA &ssa) override;
  void GenerateOn(mid::ConstZeroSSA &ssa) override;

  void Dump(std::ostream &os) const override;

  // getters
  const ModulePtr &module() const { return module_; }

 private:
  // generate code or get from metadata
  llvm::Value *GetVal(const mid::SSAPtr &ssa);
  // store llvm value to metadata
  void SetVal(mid::Value &ssa, llvm::Value *val);
  // create global ctor array
  void CreateCtorArray(llvm::Function *ctor);

  // generate on 'yulang::define::TypePtr'
  llvm::Type *GenerateType(const define::TypePtr &type);
  llvm::Type *GeneratePrimType(const define::TypePtr &type);
  llvm::Type *GenerateStructType(const define::TypePtr &type);
  llvm::Type *GenerateFuncType(const define::TypePtr &type);
  llvm::Type *GenerateFuncPtrType(const define::TypePtr &type);
  llvm::Type *GenerateArrayType(const define::TypePtr &type);
  llvm::Type *GeneratePointerType(const define::TypePtr &type);

  // LLVM stuffs
  llvm::LLVMContext context_;
  llvm::IRBuilder<> builder_;
  ModulePtr module_;
  // tables for storing structure types
  std::vector<std::pair<define::TypePtr, llvm::Type *>> types_;
  std::unordered_map<define::TypePtr, llvm::Type *> type_lut_;
};

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_GENERATOR_H_
