#include "back/llvm/generator.h"

#include <vector>
#include <cassert>

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Constant.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Config/llvm-config.h"

using namespace yulang::define;
using namespace yulang::mid;
using namespace yulang::back::ll;

namespace {

enum class TypeKind {
  Int, Float, Ptr,
};

// get kind of specific type
// used when generating type casting
inline TypeKind GetTypeKind(const TypePtr &type) {
  if (type->IsInteger() || type->IsBool()) {
    return TypeKind::Int;
  }
  else if (type->IsFloat()) {
    return TypeKind::Float;
  }
  else if (type->IsNull() || type->IsFunction() || type->IsArray() ||
           type->IsPointer()) {
    return TypeKind::Ptr;
  }
  else {
    assert(false);
    return TypeKind::Int;
  }
}

// convert SSA IR's linkage type to LLVM linkage type
inline llvm::GlobalValue::LinkageTypes GetLinkType(LinkageTypes link) {
  using LinkTypes = llvm::GlobalValue::LinkageTypes;
  switch (link) {
    case LinkageTypes::Internal: return LinkTypes::InternalLinkage;
    case LinkageTypes::Inline: return LinkTypes::LinkOnceODRLinkage;
    case LinkageTypes::External: return LinkTypes::ExternalLinkage;
    case LinkageTypes::GlobalCtor: return LinkTypes::InternalLinkage;
    case LinkageTypes::GlobalDtor: return LinkTypes::InternalLinkage;
    default: assert(false); return LinkTypes::InternalLinkage;
  }
}

}  // namespace

llvm::Value *LLVMGen::GetVal(const SSAPtr &ssa) {
  auto val = std::any_cast<llvm::Value *>(&ssa->metadata());
  if (!val) {
    ssa->GenerateCode(*this);
    val = std::any_cast<llvm::Value *>(&ssa->metadata());
    assert(val);
  }
  return *val;
}

void LLVMGen::SetVal(mid::Value &ssa, llvm::Value *val) {
  ssa.set_metadata(val);
}

void LLVMGen::CreateCtorArray(llvm::Function *ctor) {
  using namespace llvm;
  auto type = ctor->getType();
  auto global_ty = llvm::StructType::get(builder_.getInt32Ty(), type,
                                         builder_.getInt8PtrTy());
  auto global_arr_ty = llvm::ArrayType::get(global_ty, 1);
  auto global_init =
      ConstantStruct::get(global_ty, builder_.getInt32(65535), ctor,
                          Constant::getNullValue(builder_.getInt8PtrTy()));
  auto global_arr_init = ConstantArray::get(global_arr_ty, global_init);
  auto global_link = GlobalValue::LinkageTypes::AppendingLinkage;
  new GlobalVariable(*module_, global_arr_ty, true, global_link,
                     global_arr_init, "llvm.global_ctors");
}

llvm::Type *LLVMGen::GenerateType(const TypePtr &type) {
  // dispatcher
  if (type->IsInteger() || type->IsFloat() || type->IsBool() ||
      type->IsVoid() || type->IsNull()) {
    return GeneratePrimType(type);
  }
  else if (type->IsStruct()) {
    return GenerateStructType(type);
  }
  else if (type->IsFunction()) {
    return GenerateFuncPtrType(type);
  }
  else if (type->IsArray()) {
    return GenerateArrayType(type);
  }
  else if (type->IsPointer()) {
    return GeneratePointerType(type);
  }
  else {
    assert(false);
    return nullptr;
  }
}

llvm::Type *LLVMGen::GeneratePrimType(const TypePtr &type) {
  if (type->IsInteger()) {
    // LLVM's integer types does not distinction
    // between signed and unsigned
    return llvm::Type::getIntNTy(context_, type->GetSize() * 8);
  }
  else if (type->IsFloat()) {
    return type->GetSize() == 4 ? llvm::Type::getFloatTy(context_)
                                : llvm::Type::getDoubleTy(context_);
  }
  else if (type->IsBool()) {
    return llvm::Type::getInt1Ty(context_);
  }
  else if (type->IsVoid()) {
    return llvm::Type::getVoidTy(context_);
  }
  else {
    assert(type->IsNull());
    return llvm::Type::getInt8PtrTy(context_);
  }
}

llvm::Type *LLVMGen::GenerateStructType(const TypePtr &type) {
  // TODO: optimize
  // try to find in look up table
  auto it = type_lut_.find(type);
  if (it != type_lut_.end()) return it->second;
  // try to find in type table
  for (const auto &[ty, ll_ty] : types_) {
    if (type->IsIdentical(ty)) {
      type_lut_.insert({type, ll_ty});
      return ll_ty;
    }
  }
  // not found, create new structure type
  std::vector<llvm::Type *> elems;
  // create type of structure
  auto id = type->GetTypeId();
  auto struct_ty = llvm::StructType::create(context_, id);
  types_.push_back({type, struct_ty});
  type_lut_.insert({type, struct_ty});
  // create type of elements
  for (std::size_t i = 0; i < type->GetLength(); ++i) {
    auto elem = GenerateType(type->GetElem(i));
    elems.push_back(elem);
  }
  // update structure type
  struct_ty->setBody(elems);
  return struct_ty;
}

llvm::Type *LLVMGen::GenerateFuncType(const TypePtr &type) {
  // get return type
  auto args = type->GetArgsType();
  auto ret = GenerateType(type->GetReturnType(*args));
  // get type of parameters
  std::vector<llvm::Type *> params;
  for (const auto &i : *args) {
    params.push_back(GenerateType(i));
  }
  // create function type
  return llvm::FunctionType::get(ret, params, false);
}

llvm::Type *LLVMGen::GenerateFuncPtrType(const TypePtr &type) {
  return llvm::PointerType::get(GenerateFuncType(type), 0);
}

llvm::Type *LLVMGen::GenerateArrayType(const TypePtr &type) {
  auto base = type->GetDerefedType();
  return llvm::ArrayType::get(GenerateType(base), type->GetLength());
}

llvm::Type *LLVMGen::GeneratePointerType(const TypePtr &type) {
  auto base = type->GetDerefedType();
  return GenerateType(base)->getPointerTo();
}

void LLVMGen::GenerateOn(LoadSSA &ssa) {
  auto ptr = GetVal(ssa[0].value());
  auto ty = GenerateType(ssa.type());
  auto load = builder_.CreateLoad(ty, ptr, ssa.type()->IsVola());
#if LLVM_VERSION_MAJOR >= 10
  load->setAlignment(llvm::Align(ssa.type()->GetAlignSize()));
#else
  load->setAlignment(ssa.type()->GetAlignSize());
#endif
  SetVal(ssa, load);
}

void LLVMGen::GenerateOn(StoreSSA &ssa) {
  auto val = GetVal(ssa[0].value());
  auto ptr = GetVal(ssa[1].value());
  auto type = ssa[1].value()->type()->GetDerefedType();
  auto store = builder_.CreateStore(val, ptr, type->IsVola());
#if LLVM_VERSION_MAJOR >= 10
  store->setAlignment(llvm::Align(type->GetAlignSize()));
#else
  store->setAlignment(type->GetAlignSize());
#endif
  SetVal(ssa, store);
}

void LLVMGen::GenerateOn(AccessSSA &ssa) {
  auto ptr = GetVal(ssa[0].value());
  auto index = GetVal(ssa[1].value());
  auto ty = GenerateType(ssa[0].value()->type()->GetDerefedType());
  llvm::Value *val = nullptr;
  if (ssa.acc_type() == AccessSSA::AccessType::Pointer) {
    val = builder_.CreateInBoundsGEP(ty, ptr, index);
  }
  else {
    assert(ssa.acc_type() == AccessSSA::AccessType::Element);
    val =
        builder_.CreateInBoundsGEP(ty, ptr, {builder_.getInt32(0), index});
  }
  SetVal(ssa, val);
}

void LLVMGen::GenerateOn(BinarySSA &ssa) {
  using BinaryOp = BinarySSA::Operator;
  // get operands
  auto lhs = GetVal(ssa[0].value());
  auto rhs = GetVal(ssa[1].value());
  // generate code
  llvm::Value *val = nullptr;
  switch (ssa.op()) {
    case BinaryOp::Add: val = builder_.CreateAdd(lhs, rhs); break;
    case BinaryOp::Sub: val = builder_.CreateSub(lhs, rhs); break;
    case BinaryOp::Mul: val = builder_.CreateMul(lhs, rhs); break;
    case BinaryOp::UDiv: val = builder_.CreateUDiv(lhs, rhs); break;
    case BinaryOp::SDiv: val = builder_.CreateSDiv(lhs, rhs); break;
    case BinaryOp::URem: val = builder_.CreateURem(lhs, rhs); break;
    case BinaryOp::SRem: val = builder_.CreateSRem(lhs, rhs); break;
    case BinaryOp::Equal: val = builder_.CreateICmpEQ(lhs, rhs); break;
    case BinaryOp::NotEq: val = builder_.CreateICmpNE(lhs, rhs); break;
    case BinaryOp::ULess: val = builder_.CreateICmpULT(lhs, rhs); break;
    case BinaryOp::SLess: val = builder_.CreateICmpSLT(lhs, rhs); break;
    case BinaryOp::ULessEq: val = builder_.CreateICmpULE(lhs, rhs); break;
    case BinaryOp::SLessEq: val = builder_.CreateICmpSLE(lhs, rhs); break;
    case BinaryOp::UGreat: val = builder_.CreateICmpUGT(lhs, rhs); break;
    case BinaryOp::SGreat: val = builder_.CreateICmpSGT(lhs, rhs); break;
    case BinaryOp::UGreatEq: val = builder_.CreateICmpUGE(lhs, rhs); break;
    case BinaryOp::SGreatEq: val = builder_.CreateICmpSGE(lhs, rhs); break;
    case BinaryOp::And: val = builder_.CreateAnd(lhs, rhs); break;
    case BinaryOp::Or: val = builder_.CreateOr(lhs, rhs); break;
    case BinaryOp::Xor: val = builder_.CreateXor(lhs, rhs); break;
    case BinaryOp::Shl: val = builder_.CreateShl(lhs, rhs); break;
    case BinaryOp::LShr: val = builder_.CreateLShr(lhs, rhs); break;
    case BinaryOp::AShr: val = builder_.CreateAShr(lhs, rhs); break;
    case BinaryOp::FAdd: val = builder_.CreateFAdd(lhs, rhs); break;
    case BinaryOp::FSub: val = builder_.CreateFSub(lhs, rhs); break;
    case BinaryOp::FMul: val = builder_.CreateFMul(lhs, rhs); break;
    case BinaryOp::FDiv: val = builder_.CreateFDiv(lhs, rhs); break;
    case BinaryOp::FRem: val = builder_.CreateFRem(lhs, rhs); break;
    case BinaryOp::FEqual: val = builder_.CreateFCmpOEQ(lhs, rhs); break;
    case BinaryOp::FNotEq: val = builder_.CreateFCmpUNE(lhs, rhs); break;
    case BinaryOp::FLess: val = builder_.CreateFCmpOLT(lhs, rhs); break;
    case BinaryOp::FLessEq: val = builder_.CreateFCmpOLE(lhs, rhs); break;
    case BinaryOp::FGreat: val = builder_.CreateFCmpOGT(lhs, rhs); break;
    case BinaryOp::FGreatEq: val = builder_.CreateFCmpOGE(lhs, rhs); break;
    default: assert(false); break;
  }
  SetVal(ssa, val);
}

void LLVMGen::GenerateOn(UnarySSA &ssa) {
  using UnaryOp = UnarySSA::Operator;
  // get operand
  auto opr = GetVal(ssa[0].value());
  const auto &type = ssa[0].value()->type();
  // generate code
  llvm::Value *val = nullptr;
  switch (ssa.op()) {
    case UnaryOp::Neg: val = builder_.CreateNeg(opr); break;
    case UnaryOp::LogicNot: {
      llvm::Value *bool_val = opr;
      if (type->IsInteger()) {
        auto zero = builder_.getIntN(type->GetSize() * 8, 0);
        bool_val = builder_.CreateICmpNE(opr, zero);
      }
      val = builder_.CreateXor(bool_val, builder_.getInt1(true));
      break;
    }
    case UnaryOp::Not: val = builder_.CreateNot(opr); break;
    case UnaryOp::FNeg: val = builder_.CreateFNeg(opr); break;
    default: assert(false); break;
  }
  SetVal(ssa, val);
}

void LLVMGen::GenerateOn(CastSSA &ssa) {
  // get operand
  auto val = GetVal(ssa[0].value());
  const auto &src = ssa[0].value()->type(), &dst = ssa.type();
  // generate type casting
  llvm::Value *ret = nullptr;
  auto type = GenerateType(dst);
  auto src_kind = GetTypeKind(src);
  auto dst_kind = GetTypeKind(dst);
  if (src_kind == TypeKind::Int && dst_kind == TypeKind::Int) {
    // int -> int
    if (src->GetSize() < dst->GetSize()) {
      ret = src->IsUnsigned() ? builder_.CreateZExt(val, type)
                              : builder_.CreateSExt(val, type);
    }
    else if (src->GetSize() > dst->GetSize()) {
      // the value should not be truncated if casted to a boolean
      if (dst->IsBool()) {
        auto zero = builder_.getIntN(src->GetSize() * 8, 0);
        ret = builder_.CreateICmpNE(val, zero);
      }
      else {
        ret = builder_.CreateTrunc(val, type);
      }
    }
    else {
      // do nothing
      ret = val;
    }
  }
  else if (src_kind == TypeKind::Int && dst_kind == TypeKind::Float) {
    // int -> float
    ret = src->IsUnsigned() ? builder_.CreateUIToFP(val, type)
                            : builder_.CreateSIToFP(val, type);
  }
  else if (src_kind == TypeKind::Float && dst_kind == TypeKind::Int) {
    // float -> int
    ret = dst->IsUnsigned() ? builder_.CreateFPToUI(val, type)
                            : builder_.CreateFPToSI(val, type);
  }
  else if (src_kind == TypeKind::Float && dst_kind == TypeKind::Float) {
    // float -> float
    ret = src->GetSize() < dst->GetSize()
              ? builder_.CreateFPExt(val, type)
              : builder_.CreateFPTrunc(val, type);
  }
  else if (src_kind == TypeKind::Ptr && dst_kind == TypeKind::Ptr) {
    // ptr -> ptr
    ret = builder_.CreateBitCast(val, type);
  }
  else if (src_kind == TypeKind::Ptr && dst_kind == TypeKind::Int) {
    // ptr -> int
    ret = builder_.CreatePtrToInt(val, type);
  }
  else if (src_kind == TypeKind::Int && dst_kind == TypeKind::Ptr) {
    // int -> ptr
    ret = builder_.CreateIntToPtr(val, type);
  }
  else {
    assert(false);
  }
  SetVal(ssa, ret);
}

void LLVMGen::GenerateOn(CallSSA &ssa) {
  // get callee
  auto callee = GetVal(ssa[0].value());
  // get arguments
  std::vector<llvm::Value *> args;
  for (std::size_t i = 1; i < ssa.size(); ++i) {
    args.push_back(GetVal(ssa[i].value()));
  }
  // get function type
  auto func_ty = llvm::dyn_cast<llvm::FunctionType>(
      GenerateFuncType(ssa[0].value()->type()));
  // create call
  auto call = builder_.CreateCall(func_ty, callee, args);
  SetVal(ssa, call);
}

void LLVMGen::GenerateOn(BranchSSA &ssa) {
  using namespace llvm;
  auto cond = GetVal(ssa[0].value());
  auto true_block = dyn_cast<BasicBlock>(GetVal(ssa[1].value()));
  auto false_block = dyn_cast<BasicBlock>(GetVal(ssa[2].value()));
  auto branch = builder_.CreateCondBr(cond, true_block, false_block);
  SetVal(ssa, branch);
}

void LLVMGen::GenerateOn(JumpSSA &ssa) {
  auto target = llvm::dyn_cast<llvm::BasicBlock>(GetVal(ssa[0].value()));
  auto jump = builder_.CreateBr(target);
  SetVal(ssa, jump);
}

void LLVMGen::GenerateOn(ReturnSSA &ssa) {
  llvm::Value *val = nullptr;
  if (ssa[0].value()) val = GetVal(ssa[0].value());
  auto ret = builder_.CreateRet(val);
  SetVal(ssa, ret);
}

void LLVMGen::GenerateOn(FunctionSSA &ssa) {
  using namespace llvm;
  // get linkage type
  auto link = GetLinkType(ssa.link());
  // get function type
  auto type = dyn_cast<FunctionType>(GenerateFuncType(ssa.type()));
  // create function declaration
  auto func = Function::Create(type, link, ssa.name(), module_.get());
  SetVal(ssa, func);
  // create argument attributes
  auto args = *ssa.org_type()->GetArgsType();
  unsigned int arg_index = 0;
  for (const auto &i : args) {
    if (i->IsReference()) {
      func->addDereferenceableParamAttr(arg_index, i->GetSize());
    }
    arg_index++;
  }
  // create return value attributes
  auto ret = ssa.org_type()->GetReturnType(args);
  if (ret->IsReference()) {
    func->addRetAttr(llvm::Attribute::getWithDereferenceableBytes(
        context_, ret->GetSize()));
  }
  // register global ctor
  if (ssa.link() == LinkageTypes::GlobalCtor) CreateCtorArray(func);
  // generate function body
  if (!ssa.empty()) {
    // create & emit entry block
    auto entry = BasicBlock::Create(context_, "entry", func);
    builder_.SetInsertPoint(entry);
    // emit other blocks
    for (const auto &i : ssa) GetVal(i.value());
    // create jump
    auto first = dyn_cast<BasicBlock>(GetVal(ssa[0].value()));
    builder_.SetInsertPoint(entry);
    builder_.CreateBr(first);
  }
}

void LLVMGen::GenerateOn(GlobalVarSSA &ssa) {
  using namespace llvm;
  // get linkage type
  auto link = GetLinkType(ssa.link());
  // get initializer
  Constant *init = nullptr;
  if (ssa.init()) init = dyn_cast<Constant>(GetVal(ssa.init()));
  // create global variable
  auto type = GenerateType(ssa.type()->GetDerefedType());
  auto global = new GlobalVariable(*module_, type, !ssa.is_var(), link,
                                   nullptr, ssa.name());
  global->setInitializer(init);
  SetVal(ssa, global);
}

void LLVMGen::GenerateOn(AllocaSSA &ssa) {
  // set insert point to entry block
  auto last_block = builder_.GetInsertBlock();
  auto &entry = last_block->getParent()->getEntryBlock();
  builder_.SetInsertPoint(&entry);
  // create alloca
  auto type = ssa.type()->GetDerefedType();
  auto alloca = builder_.CreateAlloca(GenerateType(type));
#if LLVM_VERSION_MAJOR >= 10
  alloca->setAlignment(llvm::Align(type->GetAlignSize()));
#else
  alloca->setAlignment(type->GetAlignSize());
#endif
  SetVal(ssa, alloca);
  // restore insert point
  builder_.SetInsertPoint(last_block);
}

void LLVMGen::GenerateOn(BlockSSA &ssa) {
  // create new block
  auto parent = llvm::dyn_cast<llvm::Function>(GetVal(ssa.parent()));
  auto block = llvm::BasicBlock::Create(context_, ssa.name(), parent);
  SetVal(ssa, block);
  // emit current block
  auto last_block = builder_.GetInsertBlock();
  builder_.SetInsertPoint(block);
  // generate instructions
  for (const auto &i : ssa.insts()) GetVal(i);
  // restore the insert point to last block
  builder_.SetInsertPoint(last_block);
}

void LLVMGen::GenerateOn(ArgRefSSA &ssa) {
  auto func = llvm::dyn_cast<llvm::Function>(GetVal(ssa.func()));
  auto arg = func->args().begin() + ssa.index();
  SetVal(ssa, arg);
}

void LLVMGen::GenerateOn(AsmSSA &ssa) {
  auto type = llvm::FunctionType::get(builder_.getVoidTy(), false);
  auto asm_func = llvm::InlineAsm::get(type, ssa.asm_str(), "", true);
  builder_.CreateCall(asm_func);
  SetVal(ssa, nullptr);
}

void LLVMGen::GenerateOn(ConstIntSSA &ssa) {
  llvm::Value *val = nullptr;
  if (ssa.type()->IsBool()) {
    val = builder_.getInt1(!!ssa.value());
  }
  else {
    val = builder_.getIntN(ssa.type()->GetSize() * 8, ssa.value());
  }
  SetVal(ssa, val);
}

void LLVMGen::GenerateOn(ConstFloatSSA &ssa) {
  llvm::Value *val = nullptr;
  if (ssa.type()->GetSize() == 4) {
    // float32
    llvm::APFloat af(static_cast<float>(ssa.value()));
    val = llvm::ConstantFP::get(context_, af);
  }
  else {
    // float64
    llvm::APFloat af(ssa.value());
    val = llvm::ConstantFP::get(context_, af);
  }
  SetVal(ssa, val);
}

void LLVMGen::GenerateOn(ConstStrSSA &ssa) {
#if LLVM_VERSION_MAJOR >= 11
  auto val =
      builder_.CreateGlobalStringPtr(ssa.str(), "", 0, module_.get());
#else
  auto val = builder_.CreateGlobalStringPtr(ssa.str());
#endif
  SetVal(ssa, val);
}

void LLVMGen::GenerateOn(ConstStructSSA &ssa) {
  using namespace llvm;
  // generate elements
  std::vector<Constant *> elems;
  for (const auto &i : ssa) {
    auto val = GetVal(i.value());
    auto const_val = dyn_cast<Constant>(val);
    assert(const_val);
    elems.push_back(const_val);
  }
  // generate constant structure
  auto type = GenerateType(ssa.type());
  auto val = ConstantStruct::get(dyn_cast<llvm::StructType>(type), elems);
  SetVal(ssa, val);
}

void LLVMGen::GenerateOn(ConstArraySSA &ssa) {
  using namespace llvm;
  // generate elements
  std::vector<Constant *> elems;
  for (const auto &i : ssa) {
    auto val = GetVal(i.value());
    auto const_val = dyn_cast<Constant>(val);
    assert(const_val);
    elems.push_back(const_val);
  }
  // generate constant array
  auto type = GenerateType(ssa.type());
  auto val = ConstantArray::get(dyn_cast<llvm::ArrayType>(type), elems);
  SetVal(ssa, val);
}

void LLVMGen::GenerateOn(ConstZeroSSA &ssa) {
  auto type = GenerateType(ssa.type());
  auto val = llvm::ConstantAggregateZero::get(type);
  SetVal(ssa, val);
}

void LLVMGen::Dump(std::ostream &os) const {
  llvm::raw_os_ostream raw(os);
  module_->print(raw, nullptr);
}
