#ifndef YULANG_MID_SSA_H_
#define YULANG_MID_SSA_H_

#include <string>
#include <vector>
#include <cstddef>
#include <cstdint>
#include <cassert>

#include "define/type.h"
#include "mid/usedef.h"

namespace yulang::mid {

// forward declarations
class BlockSSA;
class GlobalVarSSA;

// type aliases
using BlockPtr = std::shared_ptr<BlockSSA>;
using BlockPtrList = std::vector<BlockPtr>;
using GlobalVarPtr = std::shared_ptr<GlobalVarSSA>;

// linkage types
enum class LinkageTypes {
  Internal, Inline, External, GlobalCtorDtor,
};

// load from allocation
// operands: pointer
class LoadSSA : public User {
 public:
  LoadSSA(const SSAPtr &ptr) : addr_(ptr) {
    Reserve(1);
    AddValue(ptr);
    // set SSA type
    set_type(ptr->type()->GetDerefedType());
    assert(type());
  }

  void Dump(std::ostream &os) const override;
  const SSAPtr &GetAddr() const override { return addr_; }

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
    // assertion for type checking
    assert(value->type()->IsIdentical(ptr->type()->GetDerefedType()));
  }

  void Dump(std::ostream &os) const override;
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
    // assertions for type checking
    assert(ptr->type()->IsPointer());
    assert(acc_type_ != AccessType::Element ||
           ptr->type()->GetDerefedType()->GetLength());
  }

  void Dump(std::ostream &os) const override;

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
    Add, Sub, Mul, UDiv, SDiv, URem, SRem,
    Equal, NotEq, Less, LessEq, Great, GreatEq,
    And, Or, Xor, Shl, LShr, AShr,
    // float
    FAdd, FSub, FMul, FDiv, FRem,
    FEqual, FNotEq, FLess, FLessEq, FGreat, FGreatEq,
  };

  BinarySSA(Operator op, const SSAPtr &lhs, const SSAPtr &rhs) : op_(op) {
    Reserve(2);
    AddValue(lhs);
    AddValue(rhs);
    // assertion for type checking
    assert(lhs->type()->IsIdentical(rhs->type()));
  }

  void Dump(std::ostream &os) const override;

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

  void Dump(std::ostream &os) const override;

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
    define::TypePtrList args_type;
    for (const auto &i : args) {
      AddValue(i);
      args_type.push_back(i->type());
    }
    set_type(callee->type()->GetReturnType(args_type));
    // assertion for type checking
    assert(callee->type()->IsFunction() && type());
  }

  void Dump(std::ostream &os) const override;
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
    // assertion for type checking
    assert(cond->type()->IsBool());
  }

  void Dump(std::ostream &os) const override;
};

// unconditional jump
// operands: target
class JumpSSA : public User {
 public:
  JumpSSA(const SSAPtr &target) {
    Reserve(1);
    AddValue(target);
  }

  void Dump(std::ostream &os) const override;
};

// function return
// operands: value
class ReturnSSA : public User {
 public:
  ReturnSSA(const SSAPtr &value) {
    Reserve(1);
    AddValue(value);
  }

  void Dump(std::ostream &os) const override;
};

// function definition/declaration
// operands: bb1 (entry), bb2, ...
class FunctionSSA : public User {
 public:
  FunctionSSA(LinkageTypes link, const std::string &name,
              const define::TypePtr &type)
      : link_(link), name_(name) {
    set_type(type);
    // assertion for type checking
    assert(type->IsFunction());
  }

  void Dump(std::ostream &os) const override;

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
               const define::TypePtr &type, const SSAPtr &init)
      : link_(link), name_(name) {
    Reserve(1);
    AddValue(init);
    set_type(define::MakePointer(type));
    // assertion for type checking
    assert(!type->IsVoid());
  }

  void Dump(std::ostream &os) const override;

  // setters
  void set_init(const SSAPtr &init) { uses()[0].set_value(init); }

  // getters
  LinkageTypes link() const { return link_; }
  const std::string &name() const { return name_; }
  const SSAPtr &init() const { return uses()[0].value(); }

 private:
  LinkageTypes link_;
  std::string name_;
};

// memory allocation
class AllocaSSA : public Value {
 public:
  AllocaSSA(const define::TypePtr &type) {
    set_type(define::MakePointer(type));
    // assertion for type checking
    assert(!type->IsVoid());
  }

  void Dump(std::ostream &os) const override;
};

// basic block
class BlockSSA : public Value {
 public:
  BlockSSA(const UserPtr &parent, const std::string &name)
      : parent_(parent), name_(name) {
    succs_.reserve(2);
  }

  void Dump(std::ostream &os) const override;

  // add a new predecessor
  void AddPred(const BlockPtr &pred) { preds_.push_back(pred); }
  // add a new successor
  void AddSucc(const BlockPtr &succ) { succs_.push_back(succ); }
  // add a new instruction
  void AddInst(const SSAPtr &inst) { insts_.push_back(inst); }

  // getters
  const std::string &name() const { return name_; }
  const UserPtr &parent() const { return parent_; }
  const BlockPtrList &preds() const { return preds_; }
  const BlockPtrList &succs() const { return succs_; }
  const SSAPtrList &insts() const { return insts_; }

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
      : func_(func), index_(index) {
    auto args_type = func->type()->GetArgsType();
    set_type((*args_type)[index]);
    // assertion for type checking
    assert(index < args_type->size());
  }

  void Dump(std::ostream &os) const override;

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

  void Dump(std::ostream &os) const override;

  // getters
  const std::string &asm_str() const { return asm_str_; }

 private:
  std::string asm_str_;
};

// constant integer
class ConstIntSSA : public Value {
 public:
  ConstIntSSA(std::uint64_t value, const define::TypePtr &type)
      : value_(value) {
    set_type(type);
    // assertion for type checking
    assert(type->IsInteger() || type->IsBool());
  }

  void Dump(std::ostream &os) const override;

  // getters
  std::uint64_t value() const { return value_; }

 private:
  std::uint64_t value_;
};

// constant float
class ConstFloatSSA : public Value {
 public:
  ConstFloatSSA(double value, const define::TypePtr &type)
      : value_(value) {
    set_type(type);
    // assertion for type checking
    assert(type->IsFloat());
  }

  void Dump(std::ostream &os) const override;

  // getters
  double value() const { return value_; }

 private:
  double value_;
};

// constant string
class ConstStrSSA : public Value {
 public:
  ConstStrSSA(const std::string &str, const define::TypePtr &type)
      : str_(str) {
    set_type(type);
    // assertion for type checking
    assert(type->IsPointer() && type->GetDerefedType()->IsInteger() &&
           type->GetDerefedType()->GetSize() == 1);
  }

  void Dump(std::ostream &os) const override;

  // getters
  const std::string &str() const { return str_; }

 private:
  std::string str_;
};

// constant structure
// operands: elem1, elem2, ...
class ConstStructSSA : public User {
 public:
  ConstStructSSA(const SSAPtrList &elems, const define::TypePtr &type) {
    Reserve(elems.size());
    for (int i = 0; i < elems.size(); ++i) {
      AddValue(elems[i]);
      assert(elems[i]->type()->IsIdentical(type->GetElem(i)));
    }
    set_type(type);
    // assertion for type checking
    assert(type->IsStruct());
  }

  void Dump(std::ostream &os) const override;
};

// constant array
// operands: elem1, elem2, ...
class ConstArraySSA : public User {
 public:
  ConstArraySSA(const SSAPtrList &elems, const define::TypePtr &type) {
    Reserve(elems.size());
    for (int i = 0; i < elems.size(); ++i) {
      AddValue(elems[i]);
      assert(elems[i]->type()->IsIdentical(type->GetElem(i)));
    }
    set_type(type);
    // assertion for type checking
    assert(type->IsArray());
  }

  void Dump(std::ostream &os) const override;
};

// constant zero
class ConstZeroSSA : public Value {
 public:
  ConstZeroSSA(const define::TypePtr &type) {
    set_type(type);
    // assertion for type checking
    assert(type->IsNull() || type->IsBasic() || type->IsStruct() ||
           type->IsArray());
  }

  void Dump(std::ostream &os) const override;
};

}  // namespace yulang::mid

#endif  // YULANG_MID_SSA_H_
