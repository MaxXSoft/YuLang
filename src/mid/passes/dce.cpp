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
    for (auto it = func->begin(); it != func->end(); ++it) {
      is_entry_ = it == func->begin();
      it->value()->RunPass(*this);
      // check if need to remove current block
      if (remove_flag_) {
        it->value()->logger()->LogWarning("unreachable code");
        it->set_value(nullptr);
        changed_ = true;
      }
    }
    // rearrange uses
    int len = 0;
    for (int i = 0; i < func->size(); ++i) {
      if ((*func)[i].value()) {
        (*func)[len].set_value((*func)[i].value());
        ++len;
      }
    }
    func->Resize(len);
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
    // removed blocks with no predecessors
    if (ssa.preds().empty() && !is_entry_) {
      remove_flag_ = true;
      // remove from all successors
      auto ptr = &ssa;
      for (const auto &block : ssa.succs()) {
        block->preds().remove_if([ptr](const BlockPtr &p) {
          return p.get() == ptr;
        });
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
  // set if current block is entry block
  bool is_entry_;
};

}  // namespace

// register current pass
REGISTER_PASS(DeadCodeEliminationPass, dead_code_elim, 0, false);
