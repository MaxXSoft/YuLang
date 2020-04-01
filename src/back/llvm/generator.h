#ifndef YULANG_BACK_LLVM_GENERATOR_H_
#define YULANG_BACK_LLVM_GENERATOR_H_

#include <memory>
#include <string>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "back/codegen.h"

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

  std::size_t GetPointerSize() const override;
  void Dump(std::ostream &os) const override;

 private:
  // LLVM stuffs
  llvm::LLVMContext context_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::Module> module_;
};

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_GENERATOR_H_
