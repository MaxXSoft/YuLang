#include "back/llvm/builder.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Support/raw_os_ostream.h"

#include <vector>
#include <cassert>

using namespace yulang::define;
using namespace yulang::back;
using namespace yulang::back::ll;

namespace {

//

}  // namespace

void LLVMBuilder::Init() {
  vals_ = xstl::MakeNestedMap<std::string, llvm::Value *>();
  types_ = xstl::MakeNestedMap<std::string, llvm::Type *>();
}

xstl::Guard LLVMBuilder::NewEnv() {
  vals_ = xstl::MakeNestedMap<std::string, llvm::Value *>(vals_);
  types_ = xstl::MakeNestedMap<std::string, llvm::Type *>(types_);
  return xstl::Guard([this] {
    vals_ = vals_->outer();
    types_ = types_->outer();
  });
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
  auto store = builder_.CreateStore(val, dst, is_vola);
  store->setAlignment(type->GetAlignSize());
}

void LLVMBuilder::CreateVarLet(const std::string &id, const TypePtr &type,
                               const ASTPtr &init) {
  if (vals_->is_root()) {
    // global variables/constants
    // TODO
  }
  else {
    // local variables/constants
    auto alloca = CreateAlloca(type);
    if (init) {
      auto val = LLVMCast(init->GenerateIR(*this));
      CreateStore(val, alloca, type);
    }
    vals_->AddItem(id, alloca);
  }
}

llvm::Type *LLVMBuilder::GenerateType(const TypePtr &type) {
  // dispatcher
  if (type->IsInteger() || type->IsFloat() || type->IsBool() ||
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
  else if (type->IsReference()) {
    return GenerateRefType(type);
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

IRPtr LLVMBuilder::GenerateOn(PropertyAST &ast) {
  bool global = ast.prop() == PropertyAST::Property::Public ||
                ast.prop() == PropertyAST::Property::Extern;
  link_ = global ? llvm::GlobalValue::LinkageTypes::ExternalLinkage
                 : llvm::GlobalValue::LinkageTypes::PrivateLinkage;
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
  // TODO: handle passing/returning structures/arrays
  auto env = NewEnv();
  // get linkage type
  ast.prop()->GenerateIR(*this);
  // create function type
  std::vector<llvm::Type *> args;
  for (const auto &i : ast.args()) {
    args.push_back(GenerateType(i->ast_type()));
  }
  auto ret = ast.type() ? GenerateType(ast.type()->ast_type())
                        : builder_.getVoidTy();
  auto type = llvm::FunctionType::get(ret, args, false);
  // create function declaration
  auto func = llvm::Function::Create(type, link_, ast.id(), module_.get());
  vals_->outer()->AddItem(ast.id(), func);
  if (!ast.body()) return nullptr;
  // generate arguments
  auto args_block = llvm::BasicBlock::Create(context_, "args", func);
  builder_.SetInsertPoint(args_block);
  auto arg_it = func->args().begin();
  unsigned int arg_index = llvm::AttributeList::AttrIndex::FirstArgIndex;
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
  ret_val_ = !ret->isVoidTy() ? CreateAlloca(ast.type()->ast_type())
                              : nullptr;
  if (ast.type() && ast.type()->ast_type()->IsReference()) {
    func->addDereferenceableAttr(
        llvm::AttributeList::AttrIndex::ReturnIndex,
        ast.type()->ast_type()->GetSize());
  }
  // generate body
  func_exit_ = llvm::BasicBlock::Create(context_, "func_exit", func);
  auto body_ret = ast.body()->GenerateIR(*this);
  // generate return
  if (ret_val_) {
    assert(body_ret);
    CreateStore(LLVMCast(body_ret), ret_val_, ast.type()->ast_type());
  }
  // emit 'exit' block
  builder_.CreateBr(func_exit_);
  builder_.SetInsertPoint(func_exit_);
  builder_.CreateRet(ret_val_);
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(DeclareAST &ast) {
  using namespace llvm;
  // get linkage type
  ast.prop()->GenerateIR(*this);
  // get type of declaration
  auto type = GenerateType(ast.type()->ast_type());
  Value *val = nullptr;
  if (ast.type()->ast_type()->IsFunction()) {
    // function declaration
    auto func_ty = dyn_cast<FunctionType>(type->getPointerElementType());
    val = Function::Create(func_ty, link_, ast.id(), module_.get());
  }
  else {
    assert(vals_->is_root());
    // external global variable
    auto var = new GlobalVariable(*module_, type, false, link_,
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
  // do nothing
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
    auto ir = ast.stmts()[i]->GenerateIR(*this);
    if (i == ast.stmts().size() - 1) ret = ir;
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
  auto if_type = ast.ast_type();
  llvm::Value *if_val = nullptr;
  if (!if_type->IsVoid()) if_val = CreateAlloca(if_type);
  // create conditional branch
  auto cond = LLVMCast(ast.cond()->GenerateIR(*this));
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
  return if_val ? MakeLLVM(if_val) : nullptr;
}

IRPtr LLVMBuilder::GenerateOn(WhenAST &ast) {
  // create basic blocks
  auto func = builder_.GetInsertBlock()->getParent();
  when_end_ = llvm::BasicBlock::Create(context_, "when_end", func);
  // generate expression
  when_expr_ = LLVMCast(ast.expr()->GenerateIR(*this));
  // generate return value
  auto when_type = ast.ast_type();
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
  auto cond = LLVMCast(ast.cond()->GenerateIR(*this));
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
  auto next_func = builder_.CreateLoad(vals_->GetItem(ast.next_id()));
  auto last_func = builder_.CreateLoad(vals_->GetItem(ast.last_id()));
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
  auto expr_val = LLVMCast(ast.expr()->GenerateIR(*this));
  builder_.CreateBr(cond_block);
  // emit 'cond' block
  builder_.SetInsertPoint(cond_block);
  auto last_val = builder_.CreateCall(last_func, {expr_val});
  builder_.CreateCondBr(last_val, end_block, body_block);
  // emit 'body' block
  builder_.SetInsertPoint(body_block);
  auto new_val = builder_.CreateCall(next_func, {expr_val});
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
        auto val = LLVMCast(ast.expr()->GenerateIR(*this));
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
    auto rhs = LLVMCast(i->GenerateIR(*this));
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
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(AccessAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(CastAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(UnaryAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(IndexAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(FunCallAST &ast) {
  // generate callee
  auto callee = LLVMCast(ast.expr()->GenerateIR(*this));
  // generate arguments
  std::vector<llvm::Value *> args;
  for (const auto &i : ast.args()) {
    args.push_back(LLVMCast(i->GenerateIR(*this)));
  }
  // generate function call
  auto call = builder_.CreateCall(callee, args);
  return MakeLLVM(call);
}

IRPtr LLVMBuilder::GenerateOn(IntAST &ast) {
  auto type = ast.ast_type();
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
  // generate load
  auto val = vals_->GetItem(ast.id());
  val = CreateLoad(val, ast.ast_type());
  // handle reference
  if (ast.ast_type()->IsReference() && !ast.ast_type()->IsStruct()) {
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
  /*
    TODO:
      when initializing struct in function, generate a private constant
      when initializing array in function, generate several store ops
      ...
  */
  using namespace llvm;
  // generate LLVM type
  auto type = ast.type()->ast_type();
  auto ll_ty = GenerateType(type);
  // generate value
  Value *val = nullptr;
  if (ast.IsLiteral()) {
    // generate all elements
    std::vector<Constant *> elems;
    for (const auto &i : ast.elems()) {
      auto elem = LLVMCast(i->GenerateIR(*this));
      elems.push_back(dyn_cast<Constant>(elem));
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
    // TODO
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
  return 0;
}

void LLVMBuilder::Dump(std::ostream &os) {
  llvm::raw_os_ostream raw(os);
  module_->print(raw, nullptr);
}
