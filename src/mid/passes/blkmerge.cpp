#include <unordered_set>
#include <memory>

#include "mid/pass.h"
#include "mid/passman.h"

using namespace yulang::mid;

namespace {

/*
  merge blocks with only one jump instruction,
  replace branch with two equal targets to jump

  TODO: do not skip entry blocks

  e.g.
    %0:
      ...                       %0:
      jump %1                     ...
    %1: ; preds: %0     ==>>      jump %2
      jump %2                   %2: ; preds: %0
    %2: ; preds: %1               ...
      ...

*/
class BlockMerge : public FunctionPass {
 public:
  BlockMerge() {}

  bool RunOnFunction(const UserPtr &func) override {
    changed_ = false;
    // traverse all basic blocks
    for (auto it = func->begin(); it != func->end(); ++it) {
      is_entry_ = it == func->begin();
      it->value()->RunPass(*this);
      if (op_ == Op::ReplaceBlock) {
        // replace current block with target
        it->value()->ReplaceBy(target_);
        // remove block from current function
        it->set_value(nullptr);
      }
    }
    if (changed_) func->RemoveNull();
    // release value in 'target'
    target_ = nullptr;
    return changed_;
  }

  void RunOn(BlockSSA &ssa) override {
    // check if is jump/branch instruction
    op_ = Op::Nop;
    ssa.insts().back()->RunPass(*this);
    switch (op_) {
      case Op::IsJump: {
        if (!is_entry_ && ssa.insts().size() == 1) {
          op_ = Op::ReplaceBlock;
          auto target_block = static_cast<BlockSSA *>(target_.get());
          // get new predecessor set
          std::unordered_set<SSAPtr> preds;
          for (const auto &i : *target_block) {
            if (i.value().get() != &ssa) preds.insert(i.value());
          }
          for (const auto &i : ssa) {
            preds.insert(i.value());
          }
          // apply new predecessors to target block
          target_block->Clear();
          for (const auto &i : preds) {
            target_block->AddValue(i);
          }
          changed_ = true;
        }
        break;
      }
      case Op::ReplaceWithJump: {
        // create jump instruction
        auto jump = std::make_shared<JumpSSA>(target_);
        jump->set_logger(ssa.insts().back()->logger());
        // replace last instruction with jump
        ssa.insts().back() = jump;
        changed_ = true;
        break;
      }
      default:;
    }
  }

  void RunOn(JumpSSA &ssa) override {
    target_ = ssa[0].value();
    op_ = Op::IsJump;
  }

  void RunOn(BranchSSA &ssa) override {
    if (ssa[1].value() == ssa[2].value()) {
      target_ = ssa[1].value();
      op_ = Op::ReplaceWithJump;
    }
  }

 private:
  enum class Op {
    Nop, IsJump, ReplaceBlock, ReplaceWithJump,
  };

  bool changed_, is_entry_;
  SSAPtr target_;
  Op op_;
};

}  // namespace

// register current passs
REGISTER_PASS(BlockMerge, block_merge, 1, false);
