#include "mid/pass.h"
#include "mid/passman.h"

using namespace yulang::mid;

namespace {

// dead code elimination
class DeadCodeEliminationPass : public FunctionPass {
 public:
  DeadCodeEliminationPass() {}

  bool RunOnFunction(const UserPtr &func) override {
    changed_ = false;
    // traverse all basic blocks
    for (const auto &use : *func) {
      use.value()->RunPass(*this);
    }
    return changed_;
  }

  void RunOn(BlockSSA &ssa) override {
    // traverse all instructions
    for (auto it = ssa.insts().begin(); it != ssa.insts().end();) {
      remove_flag_ = false;
      (*it)->RunPass(*this);
      // check if need to remove current instruction
      if (remove_flag_) {
        it = ssa.insts().erase(it);
        changed_ = true;
      }
      else {
        ++it;
      }
    }
  }

  void RunOn(LoadSSA &ssa) override {
    if (!ssa.type()->IsVola() && ssa.uses().empty()) {
      remove_flag_ = true;
    }
  }

  void RunOn(AccessSSA &ssa) override {
    if (ssa.uses().empty()) remove_flag_ = true;
  }

  void RunOn(BinarySSA &ssa) override {
    if (ssa.uses().empty()) remove_flag_ = true;
  }

  void RunOn(UnarySSA &ssa) override {
    if (ssa.uses().empty()) remove_flag_ = true;
  }

  void RunOn(AllocaSSA &ssa) override {
    if (ssa.uses().empty()) {
      remove_flag_ = true;
      ssa.logger()->LogWarning("unused variable definition");
    }
  }

 private:
  // set if IR changed
  bool changed_;
  // set if need to be removed
  bool remove_flag_;
};

}  // namespace

// register current pass
REGISTER_PASS(DeadCodeEliminationPass, dead_code_elim, 0, false);
