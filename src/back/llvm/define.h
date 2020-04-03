#ifndef YULANG_BACK_LLVM_DEFINE_H_
#define YULANG_BACK_LLVM_DEFINE_H_

#include "llvm/IR/Module.h"

namespace yulang::back::ll {

// pointer to LLVM module
using ModulePtr = std::unique_ptr<llvm::Module>;

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_DEFINE_H_
