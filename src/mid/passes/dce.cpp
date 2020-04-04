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
    cur_func_ = func.get();
    // traverse all basic blocks
    for (const auto &i : *func) {
      i.value()->RunPass(*this);
    }
    // rearrange uses
    func->RemoveNull();
    return changed_;
  }

  void RunOn(BlockSSA &ssa) override {
    // traverse all instructions
    for (auto it = ssa.insts().begin(); it != ssa.insts().end();) {
      remove_flag_ = false;
      (*it)->RunPass(*this);
      // check if need to be removed
      if (remove_flag_) {
        it = ssa.insts().erase(it);
        changed_ = true;
      }
      else {
        ++it;
      }
    }
    // removed non-entry blocks with no predecessors
    if (ssa.empty() && (*cur_func_)[0].value().get() != &ssa) {
      if (ssa.insts().size() > 1) {
        ssa.logger()->LogWarning("unreachable code");
      }
      // remove current block
      auto uses = ssa.uses();
      ssa.ReplaceBy(nullptr);
      // remove from all successors
      for (const auto &i : uses) {
        if (i->user() != cur_func_) i->user()->RemoveNull();
      }
      changed_ = true;
    }
  }

  void RunOn(LoadSSA &ssa) override {
    // NOTE: must remove all redundant loads, including volatile loads
    if (/* !ssa.type()->IsVola() && */ ssa.uses().empty()) {
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
  // current function
  User *cur_func_;
  // set if need to be removed
  bool remove_flag_;
};

}  // namespace

// register current pass
REGISTER_PASS(DeadCodeEliminationPass, dead_code_elim, 0, false);
