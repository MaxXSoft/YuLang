#include "back/llvm/generator.h"

#include "llvm/Support/raw_os_ostream.h"

using namespace yulang::define;
using namespace yulang::mid;
using namespace yulang::back::ll;

namespace {

//

}  // namespace

void LLVMGen::GenerateOn(LoadSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(StoreSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(AccessSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(BinarySSA &ssa) {
  //
}

void LLVMGen::GenerateOn(UnarySSA &ssa) {
  //
}

void LLVMGen::GenerateOn(CallSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(BranchSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(JumpSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(ReturnSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(FunctionSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(GlobalVarSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(AllocaSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(BlockSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(ArgRefSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(AsmSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(ConstIntSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(ConstFloatSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(ConstStrSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(ConstStructSSA &ssa) {
  //
}

void LLVMGen::GenerateOn(ConstArraySSA &ssa) {
  //
}

void LLVMGen::GenerateOn(ConstZeroSSA &ssa) {
  //
}

std::size_t LLVMGen::GetPointerSize() const {
  // TODO
  return sizeof(void *);
}

void LLVMGen::Dump(std::ostream &os) const {
  llvm::raw_os_ostream raw(os);
  module_->print(raw, nullptr);
}
