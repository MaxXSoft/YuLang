#ifndef YULANG_BACK_LLVM_IR_H_
#define YULANG_BACK_LLVM_IR_H_

#include "llvm/IR/Value.h"

#include "back/code.h"

namespace yulang::back::ll {

// wrapper of 'llvm::Value *'
class LLVMIR : public CodeInterface {
 public:
  LLVMIR(llvm::Value *value) : value_(value) {}

  std::any value() const override { return value_; }

 private:
  llvm::Value *value_;
};

// make a new LLVM IR pointer by existing LLVM IR
inline CodePtr MakeLLVM(llvm::Value *ir) {
  return std::make_shared<LLVMIR>(ir);
}

// cast code to LLVM IR
inline llvm::Value *LLVMCast(const CodePtr &code) {
  return CodeCast<llvm::Value *>(code);
}

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_IR_H_
