#include "mid/ssa.h"

#include "mid/pass.h"

using namespace yulang::mid;

void LoadSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void StoreSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void AccessSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void BinarySSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void UnarySSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void CallSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void BranchSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void JumpSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void ReturnSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void FunctionSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void GlobalVarSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void AllocaSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void BlockSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void ArgRefSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void AsmSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void ConstIntSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void ConstFloatSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void ConstStrSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void ConstStructSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void ConstArraySSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}

void ConstZeroSSA::RunPass(PassBase &pass) {
  pass.RunOn(*this);
}
