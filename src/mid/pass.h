#ifndef YULANG_MID_PASS_H_
#define YULANG_MID_PASS_H_

#include <memory>

#include "mid/ssa.h"

namespace yulang::mid {

// base class of all passes
class PassBase {
 public:
  virtual ~PassBase() = default;

  // return true if is module pass
  virtual bool IsModulePass() const = 0;
  // run on global values in module, return true if there is modification
  virtual bool RunOnModule(UserPtrList &global_vals) = 0;

  // return true if is function pass
  virtual bool IsFunctionPass() const = 0;
  // run on functions, return true if there is modification
  virtual bool RunOnFunction(const UserPtr &func) = 0;

  // return true if is block pass
  virtual bool IsBlockPass() const = 0;
  // run on basic blocks, return true if there is modification
  virtual bool RunOnBlock(const BlockPtr &block) = 0;

  // visitor methods for running on SSA IRs
  virtual void RunOn(LoadSSA &ssa) {}
  virtual void RunOn(StoreSSA &ssa) {}
  virtual void RunOn(AccessSSA &ssa) {}
  virtual void RunOn(BinarySSA &ssa) {}
  virtual void RunOn(UnarySSA &ssa) {}
  virtual void RunOn(CallSSA &ssa) {}
  virtual void RunOn(BranchSSA &ssa) {}
  virtual void RunOn(JumpSSA &ssa) {}
  virtual void RunOn(ReturnSSA &ssa) {}
  virtual void RunOn(FunctionSSA &ssa) {}
  virtual void RunOn(GlobalVarSSA &ssa) {}
  virtual void RunOn(AllocaSSA &ssa) {}
  virtual void RunOn(BlockSSA &ssa) {}
  virtual void RunOn(ArgRefSSA &ssa) {}
  virtual void RunOn(AsmSSA &ssa) {}
  virtual void RunOn(ConstIntSSA &ssa) {}
  virtual void RunOn(ConstFloatSSA &ssa) {}
  virtual void RunOn(ConstStrSSA &ssa) {}
  virtual void RunOn(ConstStructSSA &ssa) {}
  virtual void RunOn(ConstArraySSA &ssa) {}
  virtual void RunOn(ConstZeroSSA &ssa) {}
};

// pointer of pass
using PassPtr = std::unique_ptr<PassBase>;

// module pass
class ModulePass : public PassBase {
 public:
  bool IsModulePass() const override { return true; }
  bool IsFunctionPass() const override { return false; }
  bool IsBlockPass() const override { return false; }

  bool RunOnFunction(const UserPtr &funcs) override { return false; }
  bool RunOnBlock(const BlockPtr &block) override { return false; }
};

// function pass
class FunctionPass : public PassBase {
 public:
  bool IsModulePass() const override { return false; }
  bool IsFunctionPass() const override { return true; }
  bool IsBlockPass() const override { return false; }

  bool RunOnModule(UserPtrList &global_vals) override { return false; }
  bool RunOnBlock(const BlockPtr &block) override { return false; }
};

// basic block pass
class BlockPass : public PassBase {
 public:
  bool IsModulePass() const override { return false; }
  bool IsFunctionPass() const override { return false; }
  bool IsBlockPass() const override { return true; }

  bool RunOnModule(UserPtrList &global_vals) override { return false; }
  bool RunOnFunction(const UserPtr &func) override { return false; }
};

}  // namespace yulang::mid

#endif  // YULANG_MID_PASS_H_
