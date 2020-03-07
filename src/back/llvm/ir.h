#ifndef YULANG_BACK_LLVM_IR_H_
#define YULANG_BACK_LLVM_IR_H_

#include "llvm/IR/Value.h"

#include "back/ir.h"

namespace yulang::back::ll {

// wrapper of 'llvm::Value *'
class LLVMIR : public IRInterface {
 public:
  LLVMIR(llvm::Value *value) {}

  std::any value() const override { return value_; }

 private:
  llvm::Value *value_;
};

// make a new LLVM IR pointer by existing LLVM IR
inline IRPtr MakeLLVM(llvm::Value *ir) {
  return std::make_shared<LLVMIR>(ir);
}

// cast IR to LLVM IR
inline llvm::Value *LLVMCast(const IRPtr &ir) {
  return IRCast<llvm::Value *>(ir);
}

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_IR_H_
