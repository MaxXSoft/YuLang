#include "mid/pass.h"
#include "mid/passman.h"

namespace yulang::mid {

namespace {

// dead code elimination
class DeadCodeEliminationPass : public FunctionPass {
 public:
  DeadCodeEliminationPass() = default;

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
      } else {
        ++it;
      }
    }
    // removed non-entry blocks with no predecessors
    if (ssa.empty() && (*cur_func_)[0].value().get() != &ssa) {
      if (ssa.insts().size() > 1) {
        ssa.logger()->LogWarning("unreachable code");
      }
      // remove current block
      const auto uses = ssa.uses();
      ssa.ReplaceBy(nullptr);
      // remove from all successors
      for (const auto &i : uses) {
        if (i->user() != cur_func_) i->user()->RemoveNull();
      }
      changed_ = true;
    }
  }

  void RunOn(LoadSSA &ssa) override {
    // A discarded volatile value is still an observable read. Address-only
    // lvalues, such as assignment destinations, have no such side effect.
    if (ssa.uses().empty() && (!ssa.type()->IsVola() || ssa.address_only())) {
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

  void RunOn(CastSSA &ssa) override {
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
  bool changed_{};
  // current function
  User *cur_func_{};
  // set if need to be removed
  bool remove_flag_{};
};

// register current pass
// Startup registration is required; allocation failure is fatal before main.
// NOLINTNEXTLINE(bugprone-throwing-static-initialization)
REGISTER_PASS(DeadCodeEliminationPass, dead_code_elim, 0, false);

}  // namespace

}  // namespace yulang::mid
