#ifndef YULANG_MID_SSA_H_
#define YULANG_MID_SSA_H_

#include <string>
#include <list>
#include <cstddef>
#include <cstdint>

#include "mid/usedef.h"

namespace yulang::mid {

// forward declarations
class BlockSSA;
class GlobalVarSSA;

// type aliases
using BlockPtr = std::shared_ptr<BlockSSA>;
using BlockPtrList = std::list<BlockPtr>;
using GlobalVarPtr = std::shared_ptr<GlobalVarSSA>;

// linkage types
enum class LinkageTypes {
  Internal, Inline, External, GlobalCtor, GlobalDtor,
};

// load from allocation
// operands: pointer
class LoadSSA : public User {
 public:
  LoadSSA(const SSAPtr &ptr) : addr_(ptr) {
    Reserve(1);
    AddValue(ptr);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
  SSAPtr GetAddr() const override { return addr_; }

 private:
  SSAPtr addr_;
};

// store to allocation
// operands: value, pointer
class StoreSSA : public User {
 public:
  StoreSSA(const SSAPtr &value, const SSAPtr &ptr) {
    Reserve(2);
    AddValue(value);
    AddValue(ptr);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

// element accessing (load effective address)
// operands: ptr, index
class AccessSSA : public User {
 public:
  enum class AccessType { Pointer, Element };

  AccessSSA(AccessType acc_type, const SSAPtr &ptr, const SSAPtr &index)
      : acc_type_(acc_type) {
    Reserve(2);
    AddValue(ptr);
    AddValue(index);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  AccessType acc_type() const { return acc_type_; }

 private:
  AccessType acc_type_;
};

// binary operations
// operands: lhs, rhs
class BinarySSA : public User {
 public:
  enum class Operator {
    // integer
    Add, Sub, Mul, UDiv, SDiv, URem, SRem, Equal, NotEq,
    ULess, SLess, ULessEq, SLessEq, UGreat, SGreat, UGreatEq, SGreatEq,
    And, Or, Xor, Shl, LShr, AShr,
    // float
    FAdd, FSub, FMul, FDiv, FRem,
    FEqual, FNotEq, FLess, FLessEq, FGreat, FGreatEq,
  };

  BinarySSA(Operator op, const SSAPtr &lhs, const SSAPtr &rhs) : op_(op) {
    Reserve(2);
    AddValue(lhs);
    AddValue(rhs);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  Operator op() const { return op_; }

 private:
  Operator op_;
};

// unary operations
// operands: opr
class UnarySSA : public User {
 public:
  enum class Operator {
    Neg, LogicNot, Not, FNeg, Cast,
  };

  UnarySSA(Operator op, const SSAPtr &opr) : op_(op) {
    Reserve(1);
    AddValue(opr);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  Operator op() const { return op_; }

 private:
  Operator op_;
};

// function call
// operands: callee, arg1, arg2, ...
class CallSSA : public User {
 public:
  CallSSA(const SSAPtr &callee, const SSAPtrList &args) {
    Reserve(args.size() + 1);
    AddValue(callee);
    for (const auto &i : args) AddValue(i);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

// conditional branch
// operands: cond, true, false
class BranchSSA : public User {
 public:
  BranchSSA(const SSAPtr &cond, const SSAPtr &true_block,
            const SSAPtr &false_block) {
    Reserve(3);
    AddValue(cond);
    AddValue(true_block);
    AddValue(false_block);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

// unconditional jump
// operands: target
class JumpSSA : public User {
 public:
  JumpSSA(const SSAPtr &target) {
    Reserve(1);
    AddValue(target);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

// function return
// operands: value
class ReturnSSA : public User {
 public:
  ReturnSSA(const SSAPtr &value) {
    Reserve(1);
    AddValue(value);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

// function definition/declaration
// operands: bb1 (entry), bb2, ...
// NOTE: type of function SSA may be non-trivial,
// because we want to store reference information
class FunctionSSA : public User {
 public:
  FunctionSSA(LinkageTypes link, const std::string &name)
      : link_(link), name_(name) {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  LinkageTypes link() const { return link_; }
  const std::string &name() const { return name_; }

 private:
  LinkageTypes link_;
  std::string name_;
};

// global variable definition/declaration
// operands: initializer
class GlobalVarSSA : public User {
 public:
  GlobalVarSSA(LinkageTypes link, const std::string &name,
               const SSAPtr &init)
      : link_(link), name_(name) {
    Reserve(1);
    AddValue(init);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // setters
  void set_init(const SSAPtr &init) { (*this)[0].set_value(init); }

  // getters
  LinkageTypes link() const { return link_; }
  const std::string &name() const { return name_; }
  const SSAPtr &init() const { return (*this)[0].value(); }

 private:
  LinkageTypes link_;
  std::string name_;
};

// memory allocation
class AllocaSSA : public Value {
 public:
  AllocaSSA() {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

// basic block
class BlockSSA : public Value {
 public:
  BlockSSA(const UserPtr &parent, const std::string &name)
      : name_(name), parent_(parent) {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // add a new predecessor
  void AddPred(const BlockPtr &pred) { preds_.push_back(pred); }
  // add a new successor
  // successor can be 'nullptr' (when returning from function)
  void AddSucc(const BlockPtr &succ) { succs_.push_back(succ); }
  // add a new instruction
  void AddInst(const SSAPtr &inst) { insts_.push_back(inst); }

  // getters
  const std::string &name() const { return name_; }
  const UserPtr &parent() const { return parent_; }
  BlockPtrList &preds() { return preds_; }
  BlockPtrList &succs() { return succs_; }
  SSAPtrList &insts() { return insts_; }

 private:
  // block name
  std::string name_;
  // parent function
  UserPtr parent_;
  // predecessor and successor blocks
  BlockPtrList preds_, succs_;
  // instructions in current block
  SSAPtrList insts_;
};

// argument reference
class ArgRefSSA : public Value {
 public:
  ArgRefSSA(const SSAPtr &func, std::size_t index)
      : func_(func), index_(index) {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  const SSAPtr &func() const { return func_; }
  std::size_t index() const { return index_; }

 private:
  SSAPtr func_;
  std::size_t index_;
};

// inline assemble
class AsmSSA : public Value {
 public:
  AsmSSA(const std::string &asm_str) : asm_str_(asm_str) {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  const std::string &asm_str() const { return asm_str_; }

 private:
  std::string asm_str_;
};

// constant integer
class ConstIntSSA : public Value {
 public:
  ConstIntSSA(std::uint64_t value) : value_(value) {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  std::uint64_t value() const { return value_; }

 private:
  std::uint64_t value_;
};

// constant float
class ConstFloatSSA : public Value {
 public:
  ConstFloatSSA(double value) : value_(value) {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  double value() const { return value_; }

 private:
  double value_;
};

// constant string
class ConstStrSSA : public Value {
 public:
  ConstStrSSA(const std::string &str) : str_(str) {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;

  // getters
  const std::string &str() const { return str_; }

 private:
  std::string str_;
};

// constant structure
// operands: elem1, elem2, ...
class ConstStructSSA : public User {
 public:
  ConstStructSSA(const SSAPtrList &elems) {
    Reserve(elems.size());
    for (const auto &i : elems) AddValue(i);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

// constant array
// operands: elem1, elem2, ...
class ConstArraySSA : public User {
 public:
  ConstArraySSA(const SSAPtrList &elems) {
    Reserve(elems.size());
    for (const auto &i : elems) AddValue(i);
  }

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

// constant zero
class ConstZeroSSA : public Value {
 public:
  ConstZeroSSA() {}

  void Dump(std::ostream &os, IdManager &idm) const override;
  void RunPass(PassBase &pass) override;
  void GenerateCode(back::CodeGen &gen) override;
};

}  // namespace yulang::mid

#endif  // YULANG_MID_SSA_H_
