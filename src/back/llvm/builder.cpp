#include "back/llvm/builder.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constant.h"
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

llvm::AllocaInst *LLVMBuilder::CreateAlloca(llvm::Type *type) {
  auto func = builder_.GetInsertBlock()->getParent();
  auto &entry = func->getEntryBlock();
  llvm::IRBuilder<> builder(&entry, entry.begin());
  return builder.CreateAlloca(type);
}

void LLVMBuilder::CreateStore(llvm::Value *val, llvm::Value *dst,
                              const TypePtr &type) {
  auto is_vola = type->IsVola();
  builder_.CreateStore(val, dst, is_vola);
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
  is_last_global_ = ast.prop() == PropertyAST::Property::Public ||
                    ast.prop() == PropertyAST::Property::Extern;
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
  // get linkage types
  ast.prop()->GenerateIR(*this);
  auto link = is_last_global_ ? llvm::Function::ExternalLinkage
                              : llvm::Function::PrivateLinkage;
  // create function type
  std::vector<llvm::Type *> args;
  for (const auto &i : ast.args()) {
    args.push_back(GenerateType(i->ast_type()));
  }
  auto ret = ast.type() ? GenerateType(ast.type()->ast_type())
                        : builder_.getVoidTy();
  auto type = llvm::FunctionType::get(ret, args, false);
  // create function declaration
  auto func = llvm::Function::Create(type, link, ast.id(), module_.get());
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
  ret_val_ = ret->isVoidTy() ? nullptr : CreateAlloca(ret);
  if (ast.type() && ast.type()->ast_type()->IsReference()) {
    func->addDereferenceableAttr(
        llvm::AttributeList::AttrIndex::ReturnIndex,
        ast.type()->ast_type()->GetSize());
  }
  // generate body
  auto body_ret = ast.body()->GenerateIR(*this);
  // generate return
  if (ret_val_) {
    assert(body_ret);
    CreateStore(LLVMCast(body_ret), ret_val_, ast.type()->ast_type());
  }
  builder_.CreateRet(ret_val_);
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(DeclareAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(TypeAliasAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(StructAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(EnumAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ImportAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(VarElemAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(LetElemAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ArgElemAST &ast) {
  // create allocation for arguments
  auto alloca = CreateAlloca(GenerateType(ast.type()->ast_type()));
  // add to envrionment
  vals_->AddItem(ast.id(), alloca);
  return MakeLLVM(alloca);
}

IRPtr LLVMBuilder::GenerateOn(StructElemAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(EnumElemAST &ast) {
  // TODO
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
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(WhenAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(WhileAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ForInAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(AsmAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ControlAST &ast) {
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(WhenElemAST &ast) {
  // TODO
  return nullptr;
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
  // TODO
  return nullptr;
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
  if (ast.ast_type()->IsReference()) {
    // TODO
    return nullptr;
  }
  else {
    return MakeLLVM(vals_->GetItem(ast.id()));
  }
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
  // llvm::ConstantStruct::get()
  // TODO
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(PrimTypeAST &ast) {
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(UserTypeAST &ast) {
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(FuncTypeAST &ast) {
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(VolaTypeAST &ast) {
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(ArrayTypeAST &ast) {
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(PointerTypeAST &ast) {
  return nullptr;
}

IRPtr LLVMBuilder::GenerateOn(RefTypeAST &ast) {
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
