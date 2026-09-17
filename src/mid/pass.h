#ifndef YULANG_MID_PASS_H_
#define YULANG_MID_PASS_H_

#include <memory>

#include "mid/ssa.h"

namespace yulang::mid {

// base class of all passes
class PassBase {
 public:
  PassBase() = default;
  PassBase(const PassBase &) = default;
  PassBase &operator=(const PassBase &) = default;
  PassBase(PassBase &&) = default;
  PassBase &operator=(PassBase &&) = default;

  virtual ~PassBase() = default;

  // return true if is module pass
  [[nodiscard]] virtual bool IsModulePass() const = 0;
  // run on global values in module, return true if there is modification
  virtual bool RunOnModule(UserPtrList &global_vals) = 0;

  // return true if is function pass
  [[nodiscard]] virtual bool IsFunctionPass() const = 0;
  // run on functions, return true if there is modification
  virtual bool RunOnFunction(const UserPtr &func) = 0;

  // return true if is block pass
  [[nodiscard]] virtual bool IsBlockPass() const = 0;
  // run on basic blocks, return true if there is modification
  virtual bool RunOnBlock(const BlockPtr &block) = 0;

  // visitor methods for running on SSA IRs
  virtual void RunOn(LoadSSA &ssa) {}
  virtual void RunOn(StoreSSA &ssa) {}
  virtual void RunOn(AccessSSA &ssa) {}
  virtual void RunOn(BinarySSA &ssa) {}
  virtual void RunOn(UnarySSA &ssa) {}
  virtual void RunOn(CastSSA &ssa) {}
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
  [[nodiscard]] bool IsModulePass() const final { return true; }
  [[nodiscard]] bool IsFunctionPass() const final { return false; }
  [[nodiscard]] bool IsBlockPass() const final { return false; }

  bool RunOnFunction(const UserPtr & /*funcs*/) final { return false; }
  bool RunOnBlock(const BlockPtr & /*block*/) final { return false; }
};

// function pass
class FunctionPass : public PassBase {
 public:
  [[nodiscard]] bool IsModulePass() const final { return false; }
  [[nodiscard]] bool IsFunctionPass() const final { return true; }
  [[nodiscard]] bool IsBlockPass() const final { return false; }

  bool RunOnModule(UserPtrList & /*global_vals*/) final { return false; }
  bool RunOnBlock(const BlockPtr & /*block*/) final { return false; }
};

// basic block pass
class BlockPass : public PassBase {
 public:
  [[nodiscard]] bool IsModulePass() const final { return false; }
  [[nodiscard]] bool IsFunctionPass() const final { return false; }
  [[nodiscard]] bool IsBlockPass() const final { return true; }

  bool RunOnModule(UserPtrList & /*global_vals*/) final { return false; }
  bool RunOnFunction(const UserPtr & /*func*/) final { return false; }
};

}  // namespace yulang::mid

#endif  // YULANG_MID_PASS_H_
