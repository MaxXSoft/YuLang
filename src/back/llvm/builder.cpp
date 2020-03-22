#include "back/llvm/builder.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Support/raw_os_ostream.h"

#include <vector>
#include <cassert>

#include "define/token.h"

using namespace yulang::define;
using namespace yulang::back;
using namespace yulang::back::ll;

namespace {

enum class TypeCategory {
  Int, Float, Ptr
};

// get category of specific type
// used when generating type casting
inline TypeCategory GetTypeCategory(const TypePtr &type) {
  if (type->IsInteger() || type->IsBool() || type->IsEnum()) {
    return TypeCategory::Int;
  }
  else if (type->IsFloat()) {
    return TypeCategory::Float;
  }
  else if (type->IsNull() || type->IsFunction() || type->IsArray() ||
           type->IsPointer()) {
    return TypeCategory::Ptr;
  }
  else {
    assert(false);
    return TypeCategory::Int;
  }
}

}  // namespace

void LLVMBuilder::Init() {
  vals_ = xstl::MakeNestedMap<std::string, llvm::Value *>();
  types_ = xstl::MakeNestedMap<std::string, llvm::Type *>();
  ctor_ = nullptr;
  dtor_ = nullptr;
}

xstl::Guard LLVMBuilder::NewEnv() {
  vals_ = xstl::MakeNestedMap<std::string, llvm::Value *>(vals_);
  types_ = xstl::MakeNestedMap<std::string, llvm::Type *>(types_);
  return xstl::Guard([this] {
    vals_ = vals_->outer();
    types_ = types_->outer();
  });
}

xstl::Guard LLVMBuilder::EnterGlobalFunc(bool is_dtor) {
  using namespace llvm;
  auto &func = is_dtor ? dtor_ : ctor_;
  // get current insert point
  auto cur_block = builder_.GetInsertBlock();
  // initialize global function if it does not exit
  if (!func) {
    // create function
    auto link = GlobalValue::LinkageTypes::InternalLinkage;
    auto type = FunctionType::get(builder_.getVoidTy(), false);
    auto name = is_dtor ? "_$dtor" : "_$ctor";
    func = Function::Create(type, link, name, module_.get());
    // create basic blocks
    auto entry_block = BasicBlock::Create(context_, "entry", func);
    auto exit_block = BasicBlock::Create(context_, "exit", func);
    builder_.SetInsertPoint(entry_block);
    builder_.CreateBr(exit_block);
    builder_.SetInsertPoint(exit_block);
    builder_.CreateRet(nullptr);
    // create global ctor/dtor array
    auto global_ty =
        llvm::StructType::get(builder_.getInt32Ty(), type->getPointerTo(),
                              builder_.getInt8PtrTy());
    auto global_arr_ty = llvm::ArrayType::get(global_ty, 1);
    auto global_init = ConstantStruct::get(
        global_ty, builder_.getInt32(65535), func,
        Constant::getNullValue(builder_.getInt8PtrTy()));
    auto global_arr_init = ConstantArray::get(global_arr_ty, global_init);
    auto global_link = GlobalValue::LinkageTypes::AppendingLinkage;
    auto global_name = is_dtor ? "llvm.global_dtors" : "llvm.global_ctors";
    new GlobalVariable(*module_, global_arr_ty, true, global_link,
                       global_arr_init, global_name);
  }
  // switch to global function's body block
  auto &entry = func->getEntryBlock();
  builder_.SetInsertPoint(&entry, entry.begin());
  return xstl::Guard([this, cur_block] {
    builder_.SetInsertPoint(cur_block);
  });
}

llvm::Value *LLVMBuilder::UseValue(llvm::Value *val, const TypePtr &type) {
  if (type->IsStruct()) {
    return val;
  }
  else if (type->IsRightValue() && !type->IsReference()) {
    return val;
  }
  else {
    return CreateLoad(val, type);
  }
}

llvm::Value *LLVMBuilder::UseValue(const ASTPtr &ast) {
  return UseValue(LLVMCast(ast->GenerateIR(*this)), ast->ast_type());
}

llvm::Value *LLVMBuilder::CreateSizeValue(std::size_t val) {
  return builder_.getIntN(GetPointerSize() * 8, val);
}

llvm::Value *LLVMBuilder::CreateSizeValue(llvm::Value *val,
                                          const TypePtr &type) {
  assert(type->IsInteger());
  auto ptr_size = GetPointerSize();
  // handle extending/truncating
  if (type->GetSize() != ptr_size) {
    auto size_ty = builder_.getIntNTy(ptr_size * 8);
    if (type->GetSize() < ptr_size) {
      val = type->IsUnsigned() ? builder_.CreateZExt(val, size_ty)
                               : builder_.CreateSExt(val, size_ty);
    }
    else {
      val = builder_.CreateTrunc(val, size_ty);
    }
  }
  return val;
}

llvm::AllocaInst *LLVMBuilder::CreateAlloca(const TypePtr &type) {
  auto func = builder_.GetInsertBlock()->getParent();
  auto &entry = func->getEntryBlock();
  llvm::IRBuilder<> builder(&entry, entry.begin());
  auto alloca = builder.CreateAlloca(GenerateType(type));
  alloca->setAlignment(type->GetAlignSize());
  return alloca;
}

llvm::LoadInst *LLVMBuilder::CreateLoad(llvm::Value *val,
                                        const TypePtr &type) {
  auto load = builder_.CreateLoad(val);
  load->setAlignment(type->GetAlignSize());
  return load;
}

void LLVMBuilder::CreateStore(llvm::Value *val, llvm::Value *dst,
                              const TypePtr &type) {
  auto is_vola = type->IsVola();
  if (!type->IsReference() && type->IsStruct()) {
    // generate memory copy
    dst = builder_.CreateBitCast(dst, builder_.getInt8PtrTy());
    val = builder_.CreateBitCast(val, builder_.getInt8PtrTy());
    builder_.CreateMemCpy(dst, type->GetAlignSize(), val,
                          type->GetAlignSize(), type->GetSize(),
                          type->IsVola());
  }
  else {
    // generate store instruction
    auto store = builder_.CreateStore(val, dst, is_vola);
    store->setAlignment(type->GetAlignSize());
  }
}

void LLVMBuilder::CreateVarLet(const std::string &id, const TypePtr &type,
                               const ASTPtr &init) {
  using namespace llvm;
  if (vals_->is_root()) {
    // global variables/constants
    auto ty = GenerateType(type);
    auto zero = ConstantAggregateZero::get(ty);
    auto var = new GlobalVariable(*module_, ty, true, link_, zero, id);
    if (init) {
      if (init->IsLiteral() || type->IsReference()) {
        // generate constant initializer
        auto cons = dyn_cast<Constant>(LLVMCast(init->GenerateIR(*this)));
        assert(cons);
        var->setInitializer(cons);
      }
      else {
        // generate initialization instructions
        auto ctor = EnterGlobalFunc(false);
        CreateStore(UseValue(init), var, type);
      }
    }
    // add to current environment
    vals_->AddItem(id, var);
  }
  else {
    // local variables/constants
    auto alloca = CreateAlloca(type);
    if (init) {
      auto init_val = type->IsReference()
                          ? LLVMCast(init->GenerateIR(*this))
                          : UseValue(init);
      CreateStore(init_val, alloca, type);
    }
    vals_->AddItem(id, alloca);
  }
}

llvm::Value *LLVMBuilder::CreateCall(llvm::Value *callee,
                                     llvm::ArrayRef<llvm::Value *> args,
                                     const TypePtr &ret) {
  if (IsTypeRawStruct(ret)) {
    std::vector<llvm::Value *> args_with_ret;
    // create alloca for return value
    auto alloca = CreateAlloca(ret);
    // initialize real argument list
    args_with_ret.push_back(alloca);
    args_with_ret.insert(args_with_ret.end(), args.begin(), args.end());
    // create call
    builder_.CreateCall(callee, args_with_ret);
    return alloca;
  }
  else {
    auto call = builder_.CreateCall(callee, args);
    return call;
  }
}

llvm::Value *LLVMBuilder::CreateBinOp(Operator op, llvm::Value *lhs,
                                      llvm::Value *rhs,
                                      const TypePtr &lhs_ty,
                                      const TypePtr &rhs_ty) {
  if (IsOperatorAssign(op)) {
    // get value
    auto val = rhs;
    // create assignment
    if (op != Operator::Assign) {
      val = CreateBinOp(GetDeAssignedOp(op), lhs, rhs, lhs_ty, rhs_ty);
      CreateStore(val, lhs, lhs_ty);
    }
    else {
      CreateStore(UseValue(val, rhs_ty), lhs, lhs_ty);
    }
    return nullptr;
  }
  else {
    lhs = UseValue(lhs, lhs_ty);
    rhs = UseValue(rhs, rhs_ty);
    switch (op) {
      case Operator::Add: case Operator::Sub: {
        if (lhs_ty->IsPointer() || rhs_ty->IsPointer()) {
          const auto &opr_ty = lhs_ty->IsPointer() ? rhs_ty : lhs_ty;
          auto ptr = lhs_ty->IsPointer() ? lhs : rhs;
          auto opr = lhs_ty->IsPointer() ? rhs : lhs;
          // generate index
          auto index = CreateSizeValue(opr, opr_ty);
          if (op == Operator::Sub) {
            // generate negate
            index = builder_.CreateSub(CreateSizeValue(0), index);
          }
          // generate pointer operation
          return builder_.CreateInBoundsGEP(ptr, index);
        }
        else if (lhs_ty->IsInteger()) {
          return op == Operator::Add ? builder_.CreateAdd(lhs, rhs)
                                     : builder_.CreateSub(lhs, rhs);
        }
        else {  // IsFloat
          return op == Operator::Add ? builder_.CreateFAdd(lhs, rhs)
                                     : builder_.CreateFSub(lhs, rhs);
        }
      }
      case Operator::Mul: {
        return lhs_ty->IsInteger() ? builder_.CreateMul(lhs, rhs)
                                   : builder_.CreateFMul(lhs, rhs);
      }
      case Operator::Div: {
        if (lhs_ty->IsInteger()) {
          return lhs_ty->IsUnsigned() ? builder_.CreateUDiv(lhs, rhs)
                                      : builder_.CreateSDiv(lhs, rhs);
        }
        else {  // IsFloat
          return builder_.CreateFDiv(lhs, rhs);
        }
      }
      case Operator::Mod: {
        if (lhs_ty->IsInteger()) {
          return lhs_ty->IsUnsigned() ? builder_.CreateURem(lhs, rhs)
                                      : builder_.CreateSRem(lhs, rhs);
        }
        else {  // IsFloat
          return builder_.CreateFRem(lhs, rhs);
        }
      }
      case Operator::Equal: {
        return lhs_ty->IsFloat() ? builder_.CreateFCmpOEQ(lhs, rhs)
                                 : builder_.CreateICmpEQ(lhs, rhs);
      }
      case Operator::NotEqual: {
        return lhs_ty->IsFloat() ? builder_.CreateFCmpUNE(lhs, rhs)
                                 : builder_.CreateICmpNE(lhs, rhs);
      }
      case Operator::Less: {
        if (lhs_ty->IsInteger()) {
          return lhs_ty->IsUnsigned() ? builder_.CreateICmpULT(lhs, rhs)
                                      : builder_.CreateICmpSLT(lhs, rhs);
        }
        else {  // IsFloat
          return builder_.CreateFCmpOLT(lhs, rhs);
        }
      }
      case Operator::LessEqual: {
        if (lhs_ty->IsInteger()) {
          return lhs_ty->IsUnsigned() ? builder_.CreateICmpULE(lhs, rhs)
                                      : builder_.CreateICmpSLE(lhs, rhs);
        }
        else {  // IsFloat
          return builder_.CreateFCmpOLE(lhs, rhs);
        }
      }
      case Operator::Great: {
        if (lhs_ty->IsInteger()) {
          return lhs_ty->IsUnsigned() ? builder_.CreateICmpUGT(lhs, rhs)
                                      : builder_.CreateICmpSGT(lhs, rhs);
        }
        else {  // IsFloat
          return builder_.CreateFCmpOGT(lhs, rhs);
        }
      }
      case Operator::GreatEqual: {
        if (lhs_ty->IsInteger()) {
          return lhs_ty->IsUnsigned() ? builder_.CreateICmpUGE(lhs, rhs)
                                      : builder_.CreateICmpSGE(lhs, rhs);
        }
        else {  // IsFloat
          return builder_.CreateFCmpOGE(lhs, rhs);
        }
      }
      case Operator::And: return builder_.CreateAnd(lhs, rhs);
      case Operator::Or: return builder_.CreateOr(lhs, rhs);
      case Operator::Xor: return builder_.CreateXor(lhs, rhs);
      case Operator::Shl: return builder_.CreateShl(lhs, rhs);
      case Operator::Shr: {
        return lhs_ty->IsUnsigned() ? builder_.CreateLShr(lhs, rhs)
                                    : builder_.CreateAShr(lhs, rhs);
      }
      default: assert(false); return nullptr;
    }
  }
}

llvm::Type *LLVMBuilder::GenerateType(const TypePtr &type) {
  // dispatcher
  if (type->IsReference()) {
    return GenerateRefType(type);
  }
  else if (type->IsInteger() || type->IsFloat() || type->IsBool() ||
           type->IsVoid()) {
    return GeneratePrimType(type);
  }
  else if (type->IsStruct()) {
    return GenerateStructType(type);
  }
  else if (type->IsEnum()) {
    return GenerateEnumType(type);
  }
  else if (type->IsFunction()) {
    return GenerateFuncType(type);
  }
  else if (type->IsArray()) {
    return GenerateArrayType(type);
  }
  else if (type->IsPointer()) {
    return GeneratePointerType(type);
  }
  else {
    assert(false);
  }
}

llvm::Type *LLVMBuilder::GeneratePrimType(const TypePtr &type) {
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
  else {  // IsVoid
    return llvm::Type::getVoidTy(context_);
  }
}

llvm::Type *LLVMBuilder::GenerateStructType(const TypePtr &type) {
  auto id = type->GetTypeId();
  if (auto t = types_->GetItem(id)) {
    return t;
  }
  else {
    std::vector<llvm::Type *> elems;
    // create type of structure
    auto struct_ty = llvm::StructType::create(context_, id);
    types_->AddItem(id, struct_ty);
    // create type of elements
    for (int i = 0; i < type->GetLength(); ++i) {
      auto elem = GenerateType(type->GetElem(i));
      elems.push_back(elem);
    }
    // update structure type
    struct_ty->setBody(elems);
    return struct_ty;
  }
}

llvm::Type *LLVMBuilder::GenerateEnumType(const TypePtr &type) {
  // just return the base integer type of enumeration
  return llvm::Type::getIntNTy(context_, type->GetSize() * 8);
}

llvm::Type *LLVMBuilder::GenerateFuncType(const TypePtr &type) {
  // get return type
  auto args = type->GetArgsType();
  auto result = GenerateType(type->GetReturnType(*args));
  // get type of parameters
  std::vector<llvm::Type *> params;
  for (const auto &i : *args) {
    params.push_back(GenerateType(i));
  }
  // create function pointer
  auto func = llvm::FunctionType::get(result, params, false);
  return func->getPointerTo();
}

llvm::Type *LLVMBuilder::GenerateArrayType(const TypePtr &type) {
  auto base = type->GetDerefedType();
  return llvm::ArrayType::get(GenerateType(base), type->GetLength());
}

llvm::Type *LLVMBuilder::GeneratePointerType(const TypePtr &type) {
  auto base = type->GetDerefedType();
  return GenerateType(base)->getPointerTo();
}

llvm::Type *LLVMBuilder::GenerateRefType(const TypePtr &type) {
  // treat references as pointers
  return GeneratePointerType(type);
}

bool LLVMBuilder::IsTypeRawStruct(const TypePtr &type) {
  return type->IsStruct() && !type->IsReference();
}

IRPtr LLVMBuilder::GenerateOn(PropertyAST &ast) {
  bool global = ast.prop() == PropertyAST::Property::Public ||
                ast.prop() == PropertyAST::Property::Extern;
  link_ = global ? llvm::GlobalValue::LinkageTypes::ExternalLinkage
                 : llvm::GlobalValue::LinkageTypes::InternalLinkage;
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(VarLetDefAST &ast) {
  ast.prop()->GenerateIR(*this);
  for (const auto &i : ast.defs()) {
    i->GenerateIR(*this);
  }
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(FunDefAST &ast) {
  auto env = NewEnv();
  // get linkage type
  ast.prop()->GenerateIR(*this);
  if (!ast.body()) {
    link_ = llvm::GlobalValue::LinkageTypes::ExternalLinkage;
  }
  // create function type
  auto ret_ty = ast.type() ? ast.type()->ast_type() : MakeVoid();
  auto is_ret_arg = IsTypeRawStruct(ret_ty);
  auto ret = !is_ret_arg ? GenerateType(ret_ty) : builder_.getVoidTy();
  std::vector<llvm::Type *> args;
  if (is_ret_arg) {
    args.push_back(GenerateType(ret_ty)->getPointerTo());
  }
  for (const auto &i : ast.args()) {
    auto arg = GenerateType(i->ast_type());
    if (IsTypeRawStruct(i->ast_type())) arg = arg->getPointerTo();
    args.push_back(arg);
  }
  auto type = llvm::FunctionType::get(ret, args, false);
  // create function declaration
  auto func = llvm::Function::Create(type, link_, ast.id(), module_.get());
  vals_->outer()->AddItem(ast.id(), func);
  if (!ast.body()) return nullptr;
  // generate arguments
  auto args_block = llvm::BasicBlock::Create(context_, "args", func);
  builder_.SetInsertPoint(args_block);
  unsigned int arg_index = llvm::AttributeList::AttrIndex::FirstArgIndex;
  auto arg_it = func->args().begin();
  if (is_ret_arg) ret_val_ = arg_it++;
  for (const auto &i : ast.args()) {
    auto arg = LLVMCast(i->GenerateIR(*this));
    CreateStore(arg_it, arg, i->ast_type());
    // add dereferenceable attribute to argument
    if (i->ast_type()->IsReference()) {
      func->addDereferenceableAttr(arg_index, i->ast_type()->GetSize());
    }
    ++arg_index;
    ++arg_it;
  }
  // generate return value
  if (!is_ret_arg) {
    ret_val_ = !ret_ty->IsVoid() ? CreateAlloca(ret_ty) : nullptr;
    if (ret_ty->IsReference()) {
      func->addDereferenceableAttr(
          llvm::AttributeList::AttrIndex::ReturnIndex, ret_ty->GetSize());
    }
  }
  // generate body
  func_exit_ = llvm::BasicBlock::Create(context_, "func_exit", func);
  auto body_ret = ast.body()->GenerateIR(*this);
  // generate return
  if (!ret_ty->IsVoid()) {
    assert(body_ret);
    auto val = LLVMCast(body_ret);
    if (!ret_ty->IsReference()) {
      val = UseValue(val, ast.body()->ast_type());
    }
    CreateStore(val, ret_val_, ret_ty);
  }
  // emit 'exit' block
  builder_.CreateBr(func_exit_);
  builder_.SetInsertPoint(func_exit_);
  if (is_ret_arg) {
    builder_.CreateRet(nullptr);
  }
  else {
    auto val = UseValue(ret_val_, ret_ty);
    if (ret_ty->IsStruct()) val = CreateLoad(val, ret_ty);
    builder_.CreateRet(val);
  }
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(DeclareAST &ast) {
  using namespace llvm;
  // get linkage type
  auto link = llvm::GlobalValue::LinkageTypes::ExternalLinkage;
  // get type of declaration
  auto type = GenerateType(ast.type()->ast_type());
  Value *val = nullptr;
  if (ast.type()->ast_type()->IsFunction()) {
    // function declaration
    auto func_ty = dyn_cast<FunctionType>(type->getPointerElementType());
    val = Function::Create(func_ty, link, ast.id(), module_.get());
  }
  else {
    assert(vals_->is_root());
    // external global variable
    auto var = new GlobalVariable(*module_, type, false, link,
                                  nullptr, ast.id());
    var->setAlignment(ast.type()->ast_type()->GetAlignSize());
    val = var;
  }
  // add to environment
  vals_->AddItem(ast.id(), val);
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(TypeAliasAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(StructAST &ast) {
  // generate struct definition in current type environment
  GenerateType(ast.ast_type());
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(EnumAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ImportAST &ast) {
  for (const auto &i : ast.defs()) i->GenerateIR(*this);
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(VarElemAST &ast) {
  CreateVarLet(ast.id(), ast.ast_type(), ast.init());
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(LetElemAST &ast) {
  CreateVarLet(ast.id(), ast.ast_type(), ast.init());
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ArgElemAST &ast) {
  // create allocation for arguments
  auto alloca = CreateAlloca(ast.type()->ast_type());
  // add to envrionment
  vals_->AddItem(ast.id(), alloca);
  return MakeLLVM(alloca);
}

IRPtr LLVMBuilder::GenerateOn(StructElemAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(EnumElemAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(BlockAST &ast) {
  auto env = NewEnv();
  // create new block
  auto cur_func = builder_.GetInsertBlock()->getParent();
  auto block = llvm::BasicBlock::Create(context_, "", cur_func);
  builder_.CreateBr(block);
  builder_.SetInsertPoint(block);
  // generate statements
  IRPtr ret;
  for (int i = 0; i < ast.stmts().size(); ++i) {
    const auto &stmt = ast.stmts()[i];
    auto ir = stmt->GenerateIR(*this);
    if (i == ast.stmts().size() - 1 && ir) {
      llvm::Value *val = LLVMCast(ir);
      if (!stmt->ast_type()->IsReference()) {
        val = UseValue(val, stmt->ast_type());
      }
      ret = MakeLLVM(val);
    }
  }
  return ret;
}

IRPtr LLVMBuilder::GenerateOn(IfAST &ast) {
  // create basic blocks
  auto func = builder_.GetInsertBlock()->getParent();
  auto then_block = llvm::BasicBlock::Create(context_, "if_then", func);
  auto else_block = llvm::BasicBlock::Create(context_, "if_else", func);
  auto end_block = llvm::BasicBlock::Create(context_, "if_end", func);
  // create return value of if statement
  const auto &if_type = ast.ast_type();
  llvm::Value *if_val = nullptr;
  if (!if_type->IsVoid()) if_val = CreateAlloca(if_type);
  // create conditional branch
  auto cond = UseValue(ast.cond());
  builder_.CreateCondBr(cond, then_block, else_block);
  // emit 'then' block
  builder_.SetInsertPoint(then_block);
  auto then_val = ast.then()->GenerateIR(*this);
  if (if_val) CreateStore(LLVMCast(then_val), if_val, if_type);
  builder_.CreateBr(end_block);
  // emit 'else' block
  builder_.SetInsertPoint(else_block);
  if (ast.else_then()) {
    auto else_val = ast.else_then()->GenerateIR(*this);
    if (if_val) CreateStore(LLVMCast(else_val), if_val, if_type);
  }
  builder_.CreateBr(end_block);
  // emit 'end' block
  builder_.SetInsertPoint(end_block);
  if (if_val) if_val = CreateLoad(if_val, if_type);
  return if_val ? MakeLLVM(if_val) : nullptr;
}

IRPtr LLVMBuilder::GenerateOn(WhenAST &ast) {
  // create basic blocks
  auto func = builder_.GetInsertBlock()->getParent();
  when_end_ = llvm::BasicBlock::Create(context_, "when_end", func);
  // generate expression
  when_expr_ = UseValue(ast.expr());
  // generate return value
  const auto &when_type = ast.ast_type();
  llvm::Value *when_val = nullptr;
  if (!when_type->IsVoid()) when_val = CreateAlloca(when_type);
  // generate elements
  for (const auto &i : ast.elems()) {
    auto elem = i->GenerateIR(*this);
    if (when_val) CreateStore(LLVMCast(elem), when_val, when_type);
  }
  // generate else branch
  if (ast.else_then()) {
    auto else_val = ast.else_then()->GenerateIR(*this);
    if (when_val) CreateStore(LLVMCast(else_val), when_val, when_type);
  }
  // emit 'end' block
  builder_.CreateBr(when_end_);
  builder_.SetInsertPoint(when_end_);
  if (when_val) when_val = CreateLoad(when_val, when_type);
  return when_val ? MakeLLVM(when_val) : nullptr;
}

IRPtr LLVMBuilder::GenerateOn(WhileAST &ast) {
  // create basic blocks
  auto func = builder_.GetInsertBlock()->getParent();
  auto cond_block = llvm::BasicBlock::Create(context_, "while_cond", func);
  auto body_block = llvm::BasicBlock::Create(context_, "while_body", func);
  auto end_block = llvm::BasicBlock::Create(context_, "while_end", func);
  // add to break/continue stack
  break_cont_.push({end_block, cond_block});
  // create direct branch
  builder_.CreateBr(cond_block);
  // emit 'cond' block
  builder_.SetInsertPoint(cond_block);
  auto cond = UseValue(ast.cond());
  builder_.CreateCondBr(cond, body_block, end_block);
  // emit 'body' block
  builder_.SetInsertPoint(body_block);
  ast.body()->GenerateIR(*this);
  builder_.CreateBr(cond_block);
  // emit 'end' block
  builder_.SetInsertPoint(end_block);
  // pop the top element of break/continue stack
  break_cont_.pop();
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ForInAST &ast) {
  // get iterator function
  // TODO: alignment?
  auto next_func = vals_->GetItem(ast.next_id());
  auto last_func = vals_->GetItem(ast.last_id());
  // create new environment, insert loop variable
  auto env = NewEnv();
  auto loop_var = CreateAlloca(ast.id_type());
  vals_->AddItem(ast.id(), loop_var);
  // create basic blocks
  auto func = builder_.GetInsertBlock()->getParent();
  auto cond_block = llvm::BasicBlock::Create(context_, "for_cond", func);
  auto body_block = llvm::BasicBlock::Create(context_, "for_body", func);
  auto end_block = llvm::BasicBlock::Create(context_, "for_end", func);
  // add to break/continue stack
  break_cont_.push({end_block, cond_block});
  // generate expression
  auto expr_val = UseValue(ast.expr());
  builder_.CreateBr(cond_block);
  // emit 'cond' block
  builder_.SetInsertPoint(cond_block);
  auto bool_ty = MakePrimType(Keyword::Bool, true);
  auto last_val = CreateCall(last_func, {expr_val}, bool_ty);
  builder_.CreateCondBr(last_val, end_block, body_block);
  // emit 'body' block
  builder_.SetInsertPoint(body_block);
  auto new_val = CreateCall(next_func, {expr_val}, ast.id_type());
  CreateStore(new_val, loop_var, ast.id_type());
  ast.body()->GenerateIR(*this);
  builder_.CreateBr(cond_block);
  // emit 'end' block
  builder_.SetInsertPoint(end_block);
  // pop the top element of break/continue stack
  break_cont_.pop();
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(AsmAST &ast) {
  auto type = llvm::FunctionType::get(builder_.getVoidTy(), false);
  auto asm_func = llvm::InlineAsm::get(type, ast.asm_str(), "", true);
  return MakeLLVM(builder_.CreateCall(asm_func));
}

IRPtr LLVMBuilder::GenerateOn(ControlAST &ast) {
  switch (ast.type()) {
    case Keyword::Break: case Keyword::Continue: {
      // generate target
      const auto &cur = break_cont_.top();
      auto target = ast.type() == Keyword::Break ? cur.first : cur.second;
      // generate branch
      builder_.CreateBr(target);
      break;
    }
    case Keyword::Return: {
      // generate return value
      if (ast.expr()) {
        auto val = UseValue(ast.expr());
        CreateStore(val, ret_val_, ast.expr()->ast_type());
      }
      // generate branch
      builder_.CreateBr(func_exit_);
      break;
    }
    default: assert(false); break;
  }
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(WhenElemAST &ast) {
  using namespace llvm;
  // create basic blocks
  auto func = builder_.GetInsertBlock()->getParent();
  auto body_block = BasicBlock::Create(context_, "case_body", func);
  auto exit_block = BasicBlock::Create(context_, "case_exit", func);
  // generate conditions
  for (const auto &i : ast.conds()) {
    // generate right hand side value
    auto rhs = UseValue(i);
    // generate comparison
    Value *eq = nullptr;
    if (i->ast_type()->IsFloat()) {
      eq = builder_.CreateFCmpOEQ(when_expr_, rhs);
    }
    else {
      eq = builder_.CreateICmpEQ(when_expr_, rhs);
    }
    // generate branch
    auto next_block = BasicBlock::Create(context_, "next_cond", func);
    builder_.CreateCondBr(eq, body_block, next_block);
    builder_.SetInsertPoint(next_block);
  }
  // create branch to exit block
  builder_.CreateBr(exit_block);
  // generate body
  builder_.SetInsertPoint(body_block);
  auto ret = ast.body()->GenerateIR(*this);
  builder_.CreateBr(when_end_);
  // emit 'exit' block
  builder_.SetInsertPoint(exit_block);
  return ret;
}

IRPtr LLVMBuilder::GenerateOn(BinaryAST &ast) {
  // generate lhs
  auto lhs = LLVMCast(ast.lhs()->GenerateIR(*this));
  const auto &lhs_ty = ast.lhs()->ast_type();
  // get name of overloaded function
  auto op_func = ast.op_func_id();
  // check if is logic operator (perform short circuit)
  if (!op_func &&
      (ast.op() == Operator::LogicAnd || ast.op() == Operator::LogicOr)) {
    // get current function
    auto func = builder_.GetInsertBlock()->getParent();
    // generate basic blocks
    auto rhs_block = llvm::BasicBlock::Create(context_, "logic_rhs", func);
    auto end_block = llvm::BasicBlock::Create(context_, "logic_end", func);
    // generate result value
    auto result = CreateAlloca(ast.ast_type());
    if (ast.op() == Operator::LogicAnd) {
      // generate initial value of result
      CreateStore(builder_.getInt1(false), result, ast.ast_type());
      // generate branch
      builder_.CreateCondBr(lhs, rhs_block, end_block);
    }
    else {  // LogicOr
      // generate initial value of result
      CreateStore(builder_.getInt1(true), result, ast.ast_type());
      // generate branch
      builder_.CreateCondBr(lhs, end_block, rhs_block);
    }
    // emit 'rhs' block
    builder_.SetInsertPoint(rhs_block);
    auto rhs = LLVMCast(ast.rhs()->GenerateIR(*this));
    CreateStore(rhs, result, ast.ast_type());
    builder_.CreateBr(end_block);
    // emit 'end' block
    builder_.SetInsertPoint(end_block);
    return MakeLLVM(CreateLoad(result, ast.ast_type()));
  }
  // generate rhs
  auto rhs = LLVMCast(ast.rhs()->GenerateIR(*this));
  const auto &rhs_ty = ast.rhs()->ast_type();
  // try to handle operator overloading
  if (op_func) {
    // get function
    auto callee = vals_->GetItem(*op_func);
    // generate function call
    llvm::ArrayRef<llvm::Value *> args = {
      UseValue(lhs, lhs_ty),
      UseValue(rhs, rhs_ty),
    };
    auto ret = CreateCall(callee, args, ast.ast_type());
    return MakeLLVM(ret);
  }
  // normal binary operation
  auto val = CreateBinOp(ast.op(), lhs, rhs, lhs_ty, rhs_ty);
  return val ? MakeLLVM(val) : nullptr;
}

IRPtr LLVMBuilder::GenerateOn(AccessAST &ast) {
  // generate expression
  auto expr = LLVMCast(ast.expr()->GenerateIR(*this));
  const auto &expr_ty = ast.expr()->ast_type();
  assert(expr_ty->IsStruct());
  // get index of element
  auto index = expr_ty->GetElemIndex(ast.id());
  assert(index);
  // generate access operation
  auto ptr = builder_.CreateInBoundsGEP(
      expr, {builder_.getInt32(0), builder_.getInt32(*index)});
  return MakeLLVM(ptr);
}

IRPtr LLVMBuilder::GenerateOn(CastAST &ast) {
  // generate expression
  auto expr = LLVMCast(ast.expr()->GenerateIR(*this));
  const auto &expr_ty = ast.expr()->ast_type();
  const auto &type_ty = ast.type()->ast_type();
  if (!expr_ty->IsArray()) expr = UseValue(expr, expr_ty);
  // check if is redundant type casting
  if (expr_ty->IsIdentical(type_ty)) return MakeLLVM(expr);
  // handle type casting
  llvm::Value *ret = nullptr;
  auto type = GenerateType(type_ty);
  auto expr_ct = GetTypeCategory(expr_ty);
  auto type_ct = GetTypeCategory(type_ty);
  if (expr_ct == TypeCategory::Int && type_ct == TypeCategory::Int) {
    // int -> int
    if (expr_ty->GetSize() < type_ty->GetSize()) {
      ret = expr_ty->IsUnsigned() ? builder_.CreateZExt(expr, type)
                                  : builder_.CreateSExt(expr, type);
    }
    else if (expr_ty->GetSize() > type_ty->GetSize()) {
      ret = builder_.CreateTrunc(expr, type);
    }
    else {
      // do nothing
      ret = expr;
    }
  }
  else if (expr_ct == TypeCategory::Int &&
           type_ct == TypeCategory::Float) {
    // int -> float
    ret = expr_ty->IsUnsigned() ? builder_.CreateUIToFP(expr, type)
                                : builder_.CreateSIToFP(expr, type);
  }
  else if (expr_ct == TypeCategory::Float &&
           type_ct == TypeCategory::Int) {
    // float -> int
    ret = type_ty->IsUnsigned() ? builder_.CreateFPToUI(expr, type)
                                : builder_.CreateFPToSI(expr, type);
  }
  else if (expr_ct == TypeCategory::Float &&
           type_ct == TypeCategory::Float) {
    // float -> float
    ret = expr_ty->GetSize() < type_ty->GetSize()
              ? builder_.CreateFPExt(expr, type)
              : builder_.CreateFPTrunc(expr, type);
  }
  else if (expr_ct == TypeCategory::Ptr && type_ct == TypeCategory::Ptr) {
    // ptr -> ptr
    ret = builder_.CreateBitCast(expr, type);
  }
  else if (expr_ct == TypeCategory::Ptr && type_ct == TypeCategory::Int) {
    // ptr -> int
    ret = builder_.CreatePtrToInt(expr, type);
  }
  else if (expr_ct == TypeCategory::Int && type_ct == TypeCategory::Ptr) {
    // int -> ptr
    ret = builder_.CreateIntToPtr(expr, type);
  }
  else {
    assert(false);
  }
  return MakeLLVM(ret);
}

IRPtr LLVMBuilder::GenerateOn(UnaryAST &ast) {
  using UnaryOp = UnaryAST::UnaryOp;
  // generate operand
  auto opr = LLVMCast(ast.opr()->GenerateIR(*this));
  const auto &opr_ty = ast.opr()->ast_type();
  // try to handle 'address of' operator
  if (ast.op() == UnaryOp::AddrOf) return MakeLLVM(opr);
  opr = UseValue(opr, opr_ty);
  // try to handle operator overloading
  auto op_func = ast.op_func_id();
  if (op_func) {
    // get function
    auto callee = vals_->GetItem(*op_func);
    // generate function call
    auto ret = CreateCall(callee, {opr}, ast.ast_type());
    return MakeLLVM(ret);
  }
  // normal unary operation
  llvm::Value *ret = nullptr;
  switch (ast.op()) {
    case UnaryOp::Pos: ret = opr; break;
    case UnaryOp::Neg: {
      if (opr_ty->IsInteger()) {
        auto zero = builder_.getIntN(opr_ty->GetSize() * 8, 0);
        ret = builder_.CreateSub(zero, opr);
      }
      else {  // IsFloat
        auto af = opr_ty->GetSize() == 4 ? llvm::APFloat(0.f)
                                         : llvm::APFloat(0.);
        auto fp = llvm::ConstantFP::get(context_, af);
        ret = builder_.CreateFSub(fp, opr);
      }
      break;
    }
    case UnaryOp::LogicNot: {
      llvm::Value *bool_val = opr;
      if (opr_ty->IsInteger()) {
        auto zero = builder_.getIntN(opr_ty->GetSize() * 8, 0);
        bool_val = builder_.CreateICmpNE(opr, zero);
      }
      ret = builder_.CreateXor(bool_val, builder_.getInt1(true));
      break;
    }
    case UnaryOp::Not: {
      ret = builder_.CreateNot(opr);
      break;
    }
    case UnaryOp::DeRef: {
      ret = CreateLoad(opr, opr_ty->GetDerefedType());
      break;
    }
    default: assert(false); break;
  }
  assert(ret);
  return MakeLLVM(ret);
}

IRPtr LLVMBuilder::GenerateOn(IndexAST &ast) {
  const auto &expr_type = ast.expr()->ast_type();
  // generate expression
  auto expr = LLVMCast(ast.expr()->GenerateIR(*this));
  if (!expr_type->IsArray()) expr = UseValue(expr, expr_type);
  // generate index
  auto index = UseValue(ast.index());
  index = CreateSizeValue(index, ast.index()->ast_type());
  // generate indexing operation
  llvm::Value *val = nullptr;
  if (ast.expr()->ast_type()->IsArray()) {
    val = builder_.CreateInBoundsGEP(expr, {CreateSizeValue(0), index});
  }
  else {
    val = builder_.CreateInBoundsGEP(expr, index);
  }
  return MakeLLVM(val);
}

IRPtr LLVMBuilder::GenerateOn(FunCallAST &ast) {
  // generate callee
  auto callee = UseValue(ast.expr());
  // generate arguments
  std::vector<llvm::Value *> args;
  for (const auto &i : ast.args()) {
    args.push_back(UseValue(i));
  }
  // generate function call
  auto call = CreateCall(callee, args, ast.ast_type());
  return MakeLLVM(call);
}

IRPtr LLVMBuilder::GenerateOn(IntAST &ast) {
  const auto &type = ast.ast_type();
  assert(type->IsInteger());
  llvm::APInt ai(type->GetSize() * 8, ast.value(), !type->IsUnsigned());
  return MakeLLVM(builder_.getInt(ai));
}

IRPtr LLVMBuilder::GenerateOn(FloatAST &ast) {
  llvm::APFloat af(ast.value());
  return MakeLLVM(llvm::ConstantFP::get(context_, af));
}

IRPtr LLVMBuilder::GenerateOn(CharAST &ast) {
  return MakeLLVM(builder_.getInt8(ast.c()));
}

IRPtr LLVMBuilder::GenerateOn(IdAST &ast) {
  auto val = vals_->GetItem(ast.id());
  // handle references
  if (ast.ast_type()->IsReference()) {
    // generate dereference
    val = CreateLoad(val, ast.ast_type()->GetDerefedType());
  }
  return MakeLLVM(val);
}

IRPtr LLVMBuilder::GenerateOn(StringAST &ast) {
  return MakeLLVM(builder_.CreateGlobalStringPtr(ast.str()));
}

IRPtr LLVMBuilder::GenerateOn(BoolAST &ast) {
  return MakeLLVM(builder_.getInt1(ast.value()));
}

IRPtr LLVMBuilder::GenerateOn(NullAST &ast) {
  return MakeLLVM(llvm::Constant::getNullValue(builder_.getInt8PtrTy()));
}

IRPtr LLVMBuilder::GenerateOn(ValInitAST &ast) {
  using namespace llvm;
  // generate LLVM type
  const auto &type = ast.type()->ast_type();
  auto ll_ty = GenerateType(type);
  // generate value
  Value *val = nullptr;
  if (vals_->is_root() && ast.IsLiteral()) {
    // generate all elements
    std::vector<Constant *> elems;
    for (int i = 0; i < type->GetLength(); ++i) {
      llvm::Value *cur = nullptr;
      if (i < ast.elems().size()) {
        cur = UseValue(ast.elems()[i]);
      }
      else {
        auto elem_ty = GenerateType(type->GetElem(i));
        cur = llvm::ConstantAggregateZero::get(elem_ty);
      }
      elems.push_back(dyn_cast<Constant>(cur));
    }
    if (type->IsArray()) {
      // generate array constant
      val = ConstantArray::get(dyn_cast<llvm::ArrayType>(ll_ty), elems);
    }
    else {
      assert(type->IsStruct());
      // generate strructure constant
      val = ConstantStruct::get(dyn_cast<llvm::StructType>(ll_ty), elems);
    }
  }
  else {
    // create a temporary alloca
    val = CreateAlloca(type);
    // generate zero initializer
    auto dst = builder_.CreateBitCast(val, builder_.getInt8PtrTy());
    builder_.CreateMemSet(dst, builder_.getInt8(0), type->GetSize(),
                          type->GetAlignSize(), type->IsVola());
    // generate elements
    for (int i = 0; i < ast.elems().size(); ++i) {
      const auto &elem = ast.elems()[i];
      auto ptr = builder_.CreateInBoundsGEP(
          val, {builder_.getInt32(0), builder_.getInt32(i)});
      CreateStore(UseValue(elem), ptr, elem->ast_type());
    }
    // generate load
    if (type->IsArray()) val = CreateLoad(val, type);
  }
  return MakeLLVM(val);
}

IRPtr LLVMBuilder::GenerateOn(PrimTypeAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(UserTypeAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(FuncTypeAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(VolaTypeAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ArrayTypeAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(PointerTypeAST &ast) {
  // do nothing
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(RefTypeAST &ast) {
  // do nothing
  return nullptr;
}

std::size_t LLVMBuilder::GetPointerSize() const {
  // TODO
  return sizeof(void *);
}

void LLVMBuilder::Dump(std::ostream &os) {
  llvm::raw_os_ostream raw(os);
  module_->print(raw, nullptr);
}
