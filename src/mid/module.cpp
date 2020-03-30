#include "mid/module.h"

using namespace yulang::mid;
using namespace yulang::define;

#define CREATE_BINARY(op, lhs, rhs)                         \
  do {                                                      \
    const auto &type = lhs->type();                         \
    if (type->IsInteger()) {                                \
      return CreateBinary(BinaryOp::op, lhs, rhs, type);    \
    }                                                       \
    else {                                                  \
      assert(type->IsFloat());                              \
      return CreateBinary(BinaryOp::F##op, lhs, rhs, type); \
    }                                                       \
  } while (0)

#define CREATE_RELOP(op, lhs, rhs)                             \
  do {                                                         \
    auto bool_ty = MakePrimType(Keyword::Bool, true);          \
    if (lhs->type()->IsInteger()) {                            \
      return CreateBinary(BinaryOp::op, lhs, rhs, bool_ty);    \
    }                                                          \
    else {                                                     \
      assert(lhs->type()->IsFloat());                          \
      return CreateBinary(BinaryOp::F##op, lhs, rhs, bool_ty); \
    }                                                          \
  } while (0)

#define CREATE_BITOP(op, lhs, rhs)                     \
  do {                                                 \
    const auto &type = lhs->type();                    \
    assert(type->IsInteger());                         \
    return CreateBinary(BinaryOp::op, lhs, rhs, type); \
  } while (0)

namespace {

// type aliases of operators
using BinaryOp = BinarySSA::Operator;
using UnaryOp = UnarySSA::Operator;

}  // namespace

void Module::SealGlobalCtor() {
  if (!is_ctor_sealed_) {    
    SetInsertPoint(ctor_entry_);
    CreateJump(ctor_exit_);
    is_ctor_sealed_ = true;
  }
}

UserPtr Module::CreateFunction(LinkageTypes link, const std::string &name,
                              const TypePtr &type) {
  // assertion for type checking
  assert(type->IsFunction());
  // create function
  auto func = std::make_shared<FunctionSSA>(link, name);
  // NOTE: do not get trivial type, we need reference info
  func->set_type(type);
  // add to global variables
  funcs_.push_back(func);
  return func;
}

BlockPtr Module::CreateBlock(const UserPtr &parent) {
  return CreateBlock(parent, "");
}

BlockPtr Module::CreateBlock(const UserPtr &parent,
                             const std::string &name) {
  // assertion for type checking
  assert(parent && parent->type()->IsFunction());
  // create block
  auto block = std::make_shared<BlockSSA>(parent, name);
  block->set_type(nullptr);
  // update parent function
  parent->AddValue(block);
  return block;
}

SSAPtr Module::CreateArgRef(const SSAPtr &func, std::size_t index) {
  // assertion for type checking
  auto args_type = *func->type()->GetArgsType();
  assert(index < args_type.size());
  // create argument reference
  auto arg_ref = std::make_shared<ArgRefSSA>(func, index);
  arg_ref->set_type(args_type[index]->GetTrivialType());
  return arg_ref;
}

SSAPtr Module::CreateStore(const SSAPtr &value, const SSAPtr &pointer) {
  // get proper pointer
  auto ptr = pointer;
  while (!ptr->type()->GetDerefedType() ||
         !value->type()->IsIdentical(ptr->type()->GetDerefedType())) {
    ptr = ptr->GetAddr();
    assert(ptr);
  }
  // create store
  auto store = AddInst<StoreSSA>(value, ptr);
  store->set_type(nullptr);
  return store;
}

SSAPtr Module::CreateInit(const SSAPtr &value, const SSAPtr &pointer,
                          bool is_ref) {
  auto val = value;
  // handle references
  if (is_ref) {
    val = val->GetAddr();
    assert(val);
  }
  return CreateStore(val, pointer);
}

SSAPtr Module::CreateAlloca(const TypePtr &type) {
  // assertion for type checking
  assert(!type->IsVoid());
  // create allocation
  auto alloca = AddInst<AllocaSSA>();
  alloca->set_type(MakePointer(type->GetTrivialType()));
  return alloca;
}

SSAPtr Module::CreateJump(const BlockPtr &target) {
  assert(insert_point_->succs().empty());
  // create jump
  auto jump = AddInst<JumpSSA>(target);
  jump->set_type(nullptr);
  // update predecessor & successor info
  target->AddPred(insert_point_);
  insert_point_->AddSucc(target);
  return jump;
}

SSAPtr Module::CreateReturn(const SSAPtr &value) {
  assert(insert_point_->succs().empty());
  // get proper return value
  const auto &func_type = insert_point_->parent()->type();
  auto ret_type = func_type->GetReturnType(*func_type->GetArgsType());
  auto val = value;
  if (ret_type->IsReference()) {
    val = val->GetAddr();
    assert(val);
  }
  // assertion for type checking
  assert((ret_type->IsVoid() && !val) ||
         ret_type->GetTrivialType()->IsIdentical(val->type()));
  // create return
  auto ret = AddInst<ReturnSSA>(val);
  ret->set_type(nullptr);
  // update successor info
  insert_point_->AddSucc(nullptr);
  return ret;
}

GlobalVarPtr Module::CreateGlobalVar(LinkageTypes link,
                                     const std::string &name,
                                     const TypePtr &type,
                                     const SSAPtr &init) {
  // assertions for type checking
  assert(!type->IsVoid());
  auto var_type = type->GetTrivialType();
  assert(!init || !type->IsReference() ||
         var_type->IsIdentical(init->type()));
  // create global variable definition
  auto global = std::make_shared<GlobalVarSSA>(link, name, init);
  global->set_type(MakePointer(var_type));
  // add to global variables
  vars_.push_back(global);
  return global;
}

GlobalVarPtr Module::CreateGlobalVar(LinkageTypes link,
                                     const std::string &name,
                                     const TypePtr &type) {
  return CreateGlobalVar(link, name, type, nullptr);
}

SSAPtr Module::CreateBranch(const SSAPtr &cond, const BlockPtr &true_block,
                            const BlockPtr &false_block) {
  assert(insert_point_->succs().empty());
  // assertion for type checking
  assert(cond->type()->IsBool());
  // create branch
  auto branch = AddInst<BranchSSA>(cond, true_block, false_block);
  branch->set_type(nullptr);
  // update predecessor & successor info
  true_block->AddPred(insert_point_);
  false_block->AddPred(insert_point_);
  insert_point_->AddSucc(true_block);
  insert_point_->AddSucc(false_block);
  return branch;
}

SSAPtr Module::CreateLoad(const SSAPtr &ptr, bool is_ref) {
  // assertion for type checking
  assert(ptr->type()->IsPointer());
  auto val_type = ptr->type()->GetDerefedType();
  // create load
  auto load = AddInst<LoadSSA>(ptr);
  load->set_type(val_type);
  return is_ref ? CreateLoad(load, false) : load;
}

SSAPtr Module::CreateCall(const SSAPtr &callee, const SSAPtrList &args) {
  // assertion for type checking
  assert(callee->type()->IsFunction());
  auto args_type = *callee->type()->GetArgsType();
  // assertions for checking argument type
  assert(args_type.size() == args.size());
  for (int i = 0; i < args_type.size(); ++i) {
    assert(args_type[i]->IsIdentical(args[i]->type()));
  }
  // create call
  auto call = AddInst<CallSSA>(callee, args);
  auto ret_type = callee->type()->GetReturnType(args_type);
  call->set_type(ret_type->GetTrivialType());
  // create extra load if return type is reference
  if (ret_type->IsReference()) call = CreateLoad(call, false);
  return call;
}

SSAPtr Module::CreateAsm(const std::string &asm_str) {
  auto asm_inst = AddInst<AsmSSA>(asm_str);
  asm_inst->set_type(nullptr);
  return asm_inst;
}

SSAPtr Module::CreatePtrAccess(const SSAPtr &ptr, const SSAPtr &index) {
  // assertion for type checking
  assert(ptr->type()->IsPointer() && index->type()->IsInteger());
  // create access
  auto acc_type = AccessSSA::AccessType::Pointer;
  auto access = AddInst<AccessSSA>(acc_type, ptr, index);
  access->set_type(ptr->type());
  // create load
  return CreateLoad(access, false);
}

SSAPtr Module::CreateElemAccess(const SSAPtr &ptr, const SSAPtr &index,
                                const TypePtr &type) {
  // get proper pointer
  auto pointer = ptr;
  if (!pointer->type()->IsPointer()) pointer = pointer->GetAddr();
  // assertion for type checking
  assert(pointer->type()->GetDerefedType()->GetLength() &&
         index->type()->IsInteger());
  // create access
  auto acc_type = AccessSSA::AccessType::Element;
  auto access = AddInst<AccessSSA>(acc_type, pointer, index);
  access->set_type(MakePointer(type->GetTrivialType()));
  // create load
  return CreateLoad(access, type->IsReference());
}

SSAPtr Module::CreateBinary(BinaryOp op, const SSAPtr &lhs,
                            const SSAPtr &rhs, const TypePtr &type) {
  // assertion for type checking
  assert(lhs->type()->IsIdentical(rhs->type()));
  // create binary
  auto binary = AddInst<BinarySSA>(op, lhs, rhs);
  binary->set_type(type->GetTrivialType());
  return binary;
}

SSAPtr Module::CreateUnary(UnaryOp op, const SSAPtr &opr,
                           const TypePtr &type) {
  auto unary = AddInst<UnarySSA>(op, opr);
  unary->set_type(type->GetTrivialType());
  return unary;
}

SSAPtr Module::CreateEqual(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_RELOP(Equal, lhs, rhs);
}

SSAPtr Module::CreateNeg(const SSAPtr &opr) {
  const auto &type = opr->type();
  if (type->IsInteger()) {
    return CreateUnary(UnaryOp::Neg, opr, type);
  }
  else {
    assert(type->IsFloat());
    return CreateUnary(UnaryOp::FNeg, opr, type);
  }
}

SSAPtr Module::CreateAdd(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_BINARY(Add, lhs, rhs);
}

SSAPtr Module::CreateSub(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_BINARY(Sub, lhs, rhs);
}

SSAPtr Module::CreateMul(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_BINARY(Mul, lhs, rhs);
}

SSAPtr Module::CreateDiv(const SSAPtr &lhs, const SSAPtr &rhs) {
  const auto &type = lhs->type();
  if (type->IsInteger()) {
    auto op = type->IsUnsigned() ? BinaryOp::UDiv : BinaryOp::SDiv;
    return CreateBinary(op, lhs, rhs, type);
  }
  else {
    assert(type->IsFloat());
    return CreateBinary(BinaryOp::FDiv, lhs, rhs, type);
  }
}

SSAPtr Module::CreateRem(const SSAPtr &lhs, const SSAPtr &rhs) {
  const auto &type = lhs->type();
  if (type->IsInteger()) {
    auto op = type->IsUnsigned() ? BinaryOp::URem : BinaryOp::SRem;
    return CreateBinary(op, lhs, rhs, type);
  }
  else {
    assert(type->IsFloat());
    return CreateBinary(BinaryOp::FRem, lhs, rhs, type);
  }
}

SSAPtr Module::CreateNotEq(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_RELOP(NotEq, lhs, rhs);
}

SSAPtr Module::CreateLess(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_RELOP(Less, lhs, rhs);
}

SSAPtr Module::CreateLessEq(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_RELOP(LessEq, lhs, rhs);
}

SSAPtr Module::CreateGreat(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_RELOP(Great, lhs, rhs);
}

SSAPtr Module::CreateGreatEq(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_RELOP(GreatEq, lhs, rhs);
}

SSAPtr Module::CreateAnd(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_BITOP(And, lhs, rhs);
}

SSAPtr Module::CreateOr(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_BITOP(Or, lhs, rhs);
}

SSAPtr Module::CreateXor(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_BITOP(Xor, lhs, rhs);
}

SSAPtr Module::CreateShl(const SSAPtr &lhs, const SSAPtr &rhs) {
  CREATE_BITOP(Shl, lhs, rhs);
}

SSAPtr Module::CreateShr(const SSAPtr &lhs, const SSAPtr &rhs) {
  const auto &type = lhs->type();
  assert(type->IsInteger());
  auto op = type->IsUnsigned() ? BinaryOp::LShr : BinaryOp::AShr;
  return CreateBinary(op, lhs, rhs, type);
}

SSAPtr Module::CreateCast(const SSAPtr &opr, const TypePtr &type) {
  // assertion for type checking
  const auto &opr_ty = opr->type();
  auto target = type->GetTrivialType();
  assert(opr_ty->CanCastTo(target));
  // check if is redundant type casting
  if (opr_ty->IsIdentical(target)) {
    return opr;
  }
  else {
    return CreateUnary(UnaryOp::Cast, opr, target);
  }
}

SSAPtr Module::CreateLogicNot(const SSAPtr &opr) {
  // assertion for type checking
  const auto &type = opr->type();
  assert(type->IsInteger() || type->IsBool());
  // create logic not operation
  auto bool_ty = MakePrimType(Keyword::Bool, true);
  return CreateUnary(UnaryOp::LogicNot, opr, bool_ty);
}

SSAPtr Module::CreateNot(const SSAPtr &opr) {
  const auto &type = opr->type();
  assert(type->IsInteger());
  return CreateUnary(UnaryOp::Not, opr, type);
}

SSAPtr Module::GetZero(const TypePtr &type) {
  // assertion for type checking
  assert(type->IsNull() || type->IsBasic() || type->IsStruct() ||
         type->IsArray());
  // create constant zero
  auto zero = std::make_shared<ConstZeroSSA>();
  zero->set_type(type->GetTrivialType());
  return zero;
}

SSAPtr Module::GetInt(std::uint64_t value, const TypePtr &type) {
  // assertion for type checking
  assert(type->IsInteger() || type->IsBool());
  // create constant integer
  auto const_int = std::make_shared<ConstIntSSA>(value);
  const_int->set_type(type->GetTrivialType());
  return const_int;
}

SSAPtr Module::GetInt32(std::uint32_t value) {
  auto type = MakePrimType(Keyword::Int32, true);
  return GetInt(value, type);
}

SSAPtr Module::GetBool(bool value) {
  auto type = MakePrimType(Keyword::Bool, true);
  return GetInt(value, type);
}

SSAPtr Module::GetFloat(double value, const TypePtr &type) {
  // assertion for type checking
  assert(type->IsFloat());
  // create constant float
  auto const_float = std::make_shared<ConstFloatSSA>(value);
  const_float->set_type(type->GetTrivialType());
  return const_float;
}

SSAPtr Module::GetString(const std::string &str, const TypePtr &type) {
  // assertion for type checking
  assert(type->IsPointer() && type->GetDerefedType()->IsInteger() &&
         type->GetDerefedType()->GetSize() == 1);
  // create constant string
  auto const_str = std::make_shared<ConstStrSSA>(str);
  const_str->set_type(type->GetTrivialType());
  return const_str;
}

SSAPtr Module::GetStruct(const SSAPtrList &elems, const TypePtr &type) {
  // assertions for type checking
  assert(type->IsStruct() && type->GetLength() == elems.size());
  auto struct_ty = type->GetTrivialType();
  for (int i = 0; i < elems.size(); ++i) {
    assert(struct_ty->GetElem(i)->IsIdentical(elems[i]->type()));
  }
  // create constant struct
  auto const_struct = std::make_shared<ConstStructSSA>(elems);
  const_struct->set_type(struct_ty);
  return const_struct;
}

SSAPtr Module::GetArray(const SSAPtrList &elems, const TypePtr &type) {
  // assertions for type checking
  assert(type->IsArray() && type->GetLength() == elems.size());
  auto array_ty = type->GetTrivialType();
  for (const auto &i : elems) {
    assert(array_ty->GetDerefedType()->IsIdentical(i->type()));
  }
  // create constant array
  auto const_array = std::make_shared<ConstArraySSA>(elems);
  const_array->set_type(array_ty);
  return const_array;
}

xstl::Guard Module::EnterGlobalCtor() {
  // get current insert point
  auto cur_block = insert_point_;
  // initialize global function if it does not exist
  if (!global_ctor_) {
    // create function
    auto link = LinkageTypes::Internal;
    auto ty = std::make_shared<FuncType>(TypePtrList(), MakeVoid(), true);
    global_ctor_ = CreateFunction(link, "_$ctor", ty);
    // create basic blocks
    ctor_entry_ = CreateBlock(global_ctor_, "entry");
    ctor_exit_ = CreateBlock(global_ctor_, "exit");
    SetInsertPoint(ctor_exit_);
    CreateReturn(nullptr);
    // mark as not sealed
    is_ctor_sealed_ = false;
  }
  // switch to global function's body block
  insert_point_ = ctor_entry_;
  return xstl::Guard([this, cur_block] { SetInsertPoint(cur_block); });
}

void Module::Dump(std::ostream &os) const {
  IdManager idm;
  // dump global variables
  for (const auto &i : vars_) {
    i->Dump(os, idm);
    os << std::endl;
  }
  // dump global functions
  for (const auto &i : funcs_) {
    i->Dump(os, idm);
    os << std::endl;
  }
}
