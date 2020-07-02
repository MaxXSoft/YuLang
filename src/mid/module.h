#ifndef YULANG_MID_MODULE_H_
#define YULANG_MID_MODULE_H_

#include <string>
#include <ostream>
#include <utility>
#include <stack>
#include <type_traits>

#include "define/type.h"
#include "mid/ssa.h"
#include "xstl/guard.h"
#include "front/logger.h"
#include "mid/passman.h"
#include "back/codegen.h"

namespace yulang::mid {

class Module {
 public:
  Module() { Reset(); }

  // reset module status & content
  void Reset() {
    vars_.clear();
    funcs_.clear();
    global_ctor_ = nullptr;
    ctor_entry_ = nullptr;
    ctor_exit_ = nullptr;
    is_ctor_sealed_ = false;
    insert_point_ = nullptr;
  }

  // create a function declaration
  UserPtr CreateFunction(LinkageTypes link, const std::string &name,
                        const define::TypePtr &type);
  // create a basic block
  BlockPtr CreateBlock(const UserPtr &parent);
  // create a basic block with name
  BlockPtr CreateBlock(const UserPtr &parent, const std::string &name);
  // create a argument reference
  SSAPtr CreateArgRef(const SSAPtr &func, std::size_t index);
  // create a store instruction
  SSAPtr CreateStore(const SSAPtr &value, const SSAPtr &pointer);
  // create a initialization
  SSAPtr CreateInit(const SSAPtr &value, const SSAPtr &pointer,
                    bool is_ref);
  // create a allocation instruction
  SSAPtr CreateAlloca(const define::TypePtr &type);
  // create a jump instruction
  SSAPtr CreateJump(const BlockPtr &target);
  // create a return instruction
  SSAPtr CreateReturn(const SSAPtr &value);
  // create a global variable definition
  GlobalVarPtr CreateGlobalVar(LinkageTypes link, bool is_var,
                               const std::string &name,
                               const define::TypePtr &type,
                               const SSAPtr &init);
  // create a global variable declaration
  GlobalVarPtr CreateGlobalVar(LinkageTypes link, bool is_var,
                               const std::string &name,
                               const define::TypePtr &type);
  // create a branch instruction
  SSAPtr CreateBranch(const SSAPtr &cond, const BlockPtr &true_block,
                      const BlockPtr &false_block);
  // create a load instruction
  SSAPtr CreateLoad(const SSAPtr &ptr, bool is_ref);
  // create a call instruction
  SSAPtr CreateCall(const SSAPtr &callee, const SSAPtrList &args);
  // create a inline assemble instruction
  SSAPtr CreateAsm(const std::string &asm_str);
  // create a pointer access instruction
  SSAPtr CreatePtrAccess(const SSAPtr &ptr, const SSAPtr &index);
  // create a element access instruction
  SSAPtr CreateElemAccess(const SSAPtr &ptr, const SSAPtr &index,
                          const define::TypePtr &type);
  // create a binary instruction
  SSAPtr CreateBinary(BinarySSA::Operator op, const SSAPtr &lhs,
                      const SSAPtr &rhs, const define::TypePtr &type);
  // create a unary instruction
  SSAPtr CreateUnary(UnarySSA::Operator op, const SSAPtr &opr,
                     const define::TypePtr &type);

  // create equal (with type detection)
  SSAPtr CreateEqual(const SSAPtr &lhs, const SSAPtr &rhs);
  // create negate (with type detection)
  SSAPtr CreateNeg(const SSAPtr &opr);
  // create add (with type detection)
  SSAPtr CreateAdd(const SSAPtr &lhs, const SSAPtr &rhs);
  // create sub (with type detection)
  SSAPtr CreateSub(const SSAPtr &lhs, const SSAPtr &rhs);
  // create mul (with type detection)
  SSAPtr CreateMul(const SSAPtr &lhs, const SSAPtr &rhs);
  // create div (with type detection)
  SSAPtr CreateDiv(const SSAPtr &lhs, const SSAPtr &rhs);
  // create rem (with type detection)
  SSAPtr CreateRem(const SSAPtr &lhs, const SSAPtr &rhs);
  // create not equal (with type detection)
  SSAPtr CreateNotEq(const SSAPtr &lhs, const SSAPtr &rhs);
  // create less than (with type detection)
  SSAPtr CreateLess(const SSAPtr &lhs, const SSAPtr &rhs);
  // create less or equal (with type detection)
  SSAPtr CreateLessEq(const SSAPtr &lhs, const SSAPtr &rhs);
  // create great than (with type detection)
  SSAPtr CreateGreat(const SSAPtr &lhs, const SSAPtr &rhs);
  // create great or equal (with type detection)
  SSAPtr CreateGreatEq(const SSAPtr &lhs, const SSAPtr &rhs);
  // create and
  SSAPtr CreateAnd(const SSAPtr &lhs, const SSAPtr &rhs);
  // create or
  SSAPtr CreateOr(const SSAPtr &lhs, const SSAPtr &rhs);
  // create xor
  SSAPtr CreateXor(const SSAPtr &lhs, const SSAPtr &rhs);
  // create shift left
  SSAPtr CreateShl(const SSAPtr &lhs, const SSAPtr &rhs);
  // create shift right (with type detection)
  SSAPtr CreateShr(const SSAPtr &lhs, const SSAPtr &rhs);
  // create type casting
  SSAPtr CreateCast(const SSAPtr &opr, const define::TypePtr &type);
  // create logic not
  SSAPtr CreateLogicNot(const SSAPtr &opr);
  // create bitwise not
  SSAPtr CreateNot(const SSAPtr &opr);

  // create a constant zero
  SSAPtr GetZero(const define::TypePtr &type);
  // get a constant integer
  SSAPtr GetInt(std::uint64_t value, const define::TypePtr &type);
  // get a 32-bit signed constant integer
  SSAPtr GetInt32(std::uint32_t value);
  // get a constant boolean
  SSAPtr GetBool(bool value);
  // get a constant float
  SSAPtr GetFloat(double value, const define::TypePtr &type);
  // get a constant string
  SSAPtr GetString(const std::string &str, const define::TypePtr &type);
  // get a constant structure
  SSAPtr GetStruct(const SSAPtrList &elems, const define::TypePtr &type);
  // get a constant array
  SSAPtr GetArray(const SSAPtrList &elems, const define::TypePtr &type);

  // set insert point to a specific basic block
  void SetInsertPoint(const BlockPtr &block) { insert_point_ = block; }
  // get current insert point
  const BlockPtr &GetInsertPoint() const { return insert_point_; }
  // set current context (logger)
  xstl::Guard SetContext(const front::Logger &logger);
  // set insert point to global constructor
  xstl::Guard EnterGlobalCtor();

  // dump IRs in current module
  void Dump(std::ostream &os);
  // run passes on current module
  void RunPasses(PassManager &pass_man);
  // generate current module
  void GenerateCode(back::CodeGen &gen);

 private:
  // create a new SSA with current context (logger)
  template <typename T, typename... Args>
  auto MakeSSA(Args &&... args) {
    static_assert(std::is_base_of_v<Value, T>);
    auto ssa = std::make_shared<T>(std::forward<Args>(args)...);
    ssa->set_logger(loggers_.top());
    return ssa;
  }

  // create a new instruction SSA, and push into current block
  template <typename T, typename... Args>
  SSAPtr AddInst(Args &&... args) {
    auto inst = MakeSSA<T>(std::forward<Args>(args)...);
    insert_point_->AddInst(inst);
    return inst;
  }

  // seal global constructor
  void SealGlobalCtor();

  // context rerlated stuffs
  std::stack<front::LogPtr> loggers_;
  // all global variables and functions
  UserPtrList vars_, funcs_;
  // global constructor stuffs
  UserPtr global_ctor_;
  BlockPtr ctor_entry_, ctor_exit_;
  bool is_ctor_sealed_;
  // current insert point
  BlockPtr insert_point_;
};

// make a temporary module to create specific IR, for one-time use only
inline Module MakeTempModule() {
  auto mod = Module();
  auto block = std::make_shared<BlockSSA>(nullptr, "");
  mod.SetInsertPoint(block);
  return mod;
}

}  // namespace yulang::mid

#endif  // YULANG_MID_MODULE_H_
