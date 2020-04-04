#include "mid/irbuilder.h"

#include <cassert>

#include "mid/ssa.h"

using namespace yulang::mid;
using namespace yulang::define;

namespace {

// convert property to linkage type
inline LinkageTypes GetLinkageType(Property prop) {
  if (prop == Property::Public || prop == Property::Extern) {
    return LinkageTypes::External;
  }
  else if (prop == Property::Inline) {
    return LinkageTypes::Inline;
  }
  else {
    return LinkageTypes::Internal;
  }
}

}  // namespace

xstl::Guard IRBuilder::NewEnv() {
  vals_ = xstl::MakeNestedMap<std::string, SSAPtr>(vals_);
  return xstl::Guard([this] { vals_ = vals_->outer(); });
}

SSAPtr IRBuilder::CreateBinOp(Operator op, const SSAPtr &lhs,
                              const SSAPtr &rhs) {
  if (IsOperatorAssign(op)) {
    // get value
    auto val = rhs;
    // create assignment
    if (op != Operator::Assign) {
      val = CreateBinOp(GetDeAssignedOp(op), lhs, rhs);
    }
    module_.CreateStore(val, lhs);
    return nullptr;
  }
  else {
    switch (op) {
      case Operator::Add: case Operator::Sub: {
        if (lhs->type()->IsPointer() || rhs->type()->IsPointer()) {
          // generate index
          auto index = lhs->type()->IsPointer() ? rhs : lhs;
          if (op == Operator::Sub) index = module_.CreateNeg(index);
          // generate pointer operation
          const auto &ptr = lhs->type()->IsPointer() ? lhs : rhs;
          return module_.CreatePtrAccess(ptr, index);
        }
        else {
          return op == Operator::Add ? module_.CreateAdd(lhs, rhs)
                                     : module_.CreateSub(lhs, rhs);
        }
      }
      case Operator::Mul: return module_.CreateMul(lhs, rhs);
      case Operator::Div: return module_.CreateDiv(lhs, rhs);
      case Operator::Mod: return module_.CreateRem(lhs, rhs);
      case Operator::Equal: return module_.CreateEqual(lhs, rhs);
      case Operator::NotEqual: return module_.CreateNotEq(lhs, rhs);
      case Operator::Less: return module_.CreateLess(lhs, rhs);
      case Operator::LessEqual: return module_.CreateLessEq(lhs, rhs);
      case Operator::Great: return module_.CreateGreat(lhs, rhs);
      case Operator::GreatEqual: return module_.CreateGreatEq(lhs, rhs);
      case Operator::And: return module_.CreateAnd(lhs, rhs);
      case Operator::Or: return module_.CreateOr(lhs, rhs);
      case Operator::Xor: return module_.CreateXor(lhs, rhs);
      case Operator::Shl: return module_.CreateShl(lhs, rhs);
      case Operator::Shr: return module_.CreateShr(lhs, rhs);
      default: assert(false); return nullptr;
    }
  }
}

SSAPtr IRBuilder::GenerateOn(VarLetDefAST &ast) {
  auto context = module_.SetContext(ast.logger());
  last_prop_ = ast.prop();
  for (const auto &i : ast.defs()) {
    i->GenerateIR(*this);
  }
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(FunDefAST &ast) {
  auto context = module_.SetContext(ast.logger());
  auto env = NewEnv();
  // get linkage type
  auto link = GetLinkageType(ast.prop());
  if (!ast.body()) link = LinkageTypes::External;
  // create function declaration
  auto func = module_.CreateFunction(link, ast.id(), ast.ast_type());
  vals_->outer()->AddItem(ast.id(), func);
  if (!ast.body()) return nullptr;
  // generate arguments
  auto args_block = module_.CreateBlock(func, "args");
  module_.SetInsertPoint(args_block);
  std::size_t arg_index = 0;
  for (const auto &i : ast.args()) {
    auto arg = i->GenerateIR(*this);
    auto arg_ref = module_.CreateArgRef(func, arg_index++);
    // NOTE: do not use 'CreateInit' with reference specifier
    // because argument reference's type will always be right
    module_.CreateStore(arg_ref, arg);
  }
  // generate return value
  if (ast.type()) {
    ret_val_ = module_.CreateAlloca(ast.type()->ast_type());
    ret_is_ref_ = ast.type()->ast_type()->IsReference();
  }
  // generate body
  func_exit_ = module_.CreateBlock(func, "func_exit");
  auto body_ret = ast.body()->GenerateIR(*this);
  // generate return
  if (ast.type()) {
    assert(body_ret);
    module_.CreateInit(body_ret, ret_val_, ret_is_ref_);
  }
  // emit 'exit' block
  module_.CreateJump(func_exit_);
  module_.SetInsertPoint(func_exit_);
  if (ast.type()) {
    auto ret = module_.CreateLoad(ret_val_, ret_is_ref_);
    module_.CreateReturn(ret);
  }
  else {
    module_.CreateReturn(nullptr);
  }
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(DeclareAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // get linkage type
  auto link = LinkageTypes::External;
  // get type of declaration
  const auto &type = ast.type()->ast_type();
  SSAPtr val;
  if (type->IsFunction()) {
    // function declaration
    val = module_.CreateFunction(link, ast.id(), type);
  }
  else {
    assert(vals_->is_root());
    // external global variable
    val = module_.CreateGlobalVar(link, ast.id(), type);
  }
  // add to environment
  vals_->AddItem(ast.id(), val);
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(TypeAliasAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(StructAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(EnumAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(ImportAST &ast) {
  auto context = module_.SetContext(ast.logger());
  for (const auto &i : ast.defs()) i->GenerateIR(*this);
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(VarLetElemAST &ast) {
  auto context = module_.SetContext(ast.logger());
  const auto &type = ast.ast_type();
  const auto &init = ast.init();
  SSAPtr val;
  // get linkage type
  auto link = GetLinkageType(last_prop_);
  // check if is global definition
  if (vals_->is_root()) {
    // global variables/constants
    auto var = module_.CreateGlobalVar(link, ast.id(), type);
    if (init) {
      if (init->IsLiteral()) {
        // generate initializer
        auto var_init = init->GenerateIR(*this);
        var->set_init(var_init);
      }
      else {
        // generate initialization instructions
        auto ctor = module_.EnterGlobalCtor();
        module_.CreateInit(init->GenerateIR(*this), var,
                           type->IsReference());
      }
    }
    else if (link != LinkageTypes::External) {
      // generate zero initializer
      var->set_init(module_.GetZero(type));
    }
    val = var;
  }
  else {
    // local variables/constants
    auto alloca = module_.CreateAlloca(type);
    if (init) {
      module_.CreateInit(init->GenerateIR(*this), alloca,
                         type->IsReference());
    }
    val = alloca;
  }
  // add to environment
  vals_->AddItem(ast.id(), val);
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(ArgElemAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // create allocation for arguments
  auto alloca = module_.CreateAlloca(ast.type()->ast_type());
  // add to envrionment
  vals_->AddItem(ast.id(), alloca);
  return alloca;
}

SSAPtr IRBuilder::GenerateOn(StructElemAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(EnumElemAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(BlockAST &ast) {
  auto context = module_.SetContext(ast.logger());
  auto env = NewEnv();
  // create new block
  const auto &cur_func = module_.GetInsertPoint()->parent();
  auto block = module_.CreateBlock(cur_func);
  module_.CreateJump(block);
  module_.SetInsertPoint(block);
  // generate statements
  SSAPtr ret;
  for (int i = 0; i < ast.stmts().size(); ++i) {
    const auto &stmt = ast.stmts()[i];
    auto val = stmt->GenerateIR(*this);
    if (i == ast.stmts().size() - 1) ret = val;
  }
  return ret;
}

SSAPtr IRBuilder::GenerateOn(IfAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // create basic blocks
  const auto &func = module_.GetInsertPoint()->parent();
  auto then_block = module_.CreateBlock(func, "if_then");
  auto else_block = module_.CreateBlock(func, "if_else");
  auto end_block = module_.CreateBlock(func, "if_end");
  // create return value of if statement
  const auto &if_type = ast.ast_type();
  SSAPtr if_val;
  if (!if_type->IsVoid()) if_val = module_.CreateAlloca(if_type);
  // create conditional branch
  auto cond = ast.cond()->GenerateIR(*this);
  module_.CreateBranch(cond, then_block, else_block);
  // emit 'then' block
  module_.SetInsertPoint(then_block);
  auto then_val = ast.then()->GenerateIR(*this);
  if (if_val) module_.CreateInit(then_val, if_val, if_type->IsReference());
  module_.CreateJump(end_block);
  // emit 'else' block
  module_.SetInsertPoint(else_block);
  if (ast.else_then()) {
    auto else_val = ast.else_then()->GenerateIR(*this);
    if (if_val) {
      module_.CreateInit(else_val, if_val, if_type->IsReference());
    }
  }
  module_.CreateJump(end_block);
  // emit 'end' block
  module_.SetInsertPoint(end_block);
  if (if_val) {
    if_val = module_.CreateLoad(if_val, if_type->IsReference());
  }
  return if_val;
}

SSAPtr IRBuilder::GenerateOn(WhenAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // create basic blocks
  const auto &func = module_.GetInsertPoint()->parent();
  when_end_ = module_.CreateBlock(func, "when_end");
  // generate expression
  when_expr_ = ast.expr()->GenerateIR(*this);
  // generate return value
  const auto &when_type = ast.ast_type();
  when_val_ = nullptr;
  if (!when_type->IsVoid()) {
    when_val_ = module_.CreateAlloca(when_type);
    is_when_val_ref_ = when_type->IsReference();
  }
  // generate elements
  for (const auto &i : ast.elems()) i->GenerateIR(*this);
  // generate else branch
  if (ast.else_then()) {
    auto else_val = ast.else_then()->GenerateIR(*this);
    if (when_val_) {
      module_.CreateInit(else_val, when_val_, is_when_val_ref_);
    }
  }
  // emit 'end' block
  module_.CreateJump(when_end_);
  module_.SetInsertPoint(when_end_);
  if (when_val_) {
    when_val_ = module_.CreateLoad(when_val_, is_when_val_ref_);
  }
  return when_val_;
}

SSAPtr IRBuilder::GenerateOn(WhileAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // create basic blocks
  const auto &func = module_.GetInsertPoint()->parent();
  auto cond_block = module_.CreateBlock(func, "while_cond");
  auto body_block = module_.CreateBlock(func, "while_body");
  auto end_block = module_.CreateBlock(func, "while_end");
  // add to break/continue stack
  break_cont_.push({end_block, cond_block});
  // create jump
  module_.CreateJump(cond_block);
  // emit 'cond' block
  module_.SetInsertPoint(cond_block);
  auto cond = ast.cond()->GenerateIR(*this);
  module_.CreateBranch(cond, body_block, end_block);
  // emit 'body' block
  module_.SetInsertPoint(body_block);
  ast.body()->GenerateIR(*this);
  module_.CreateJump(cond_block);
  // emit 'end' block
  module_.SetInsertPoint(end_block);
  // pop the top element of break/continue stack
  break_cont_.pop();
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(ForInAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // get iterator function
  const auto &next_func = vals_->GetItem(ast.next_id());
  const auto &last_func = vals_->GetItem(ast.last_id());
  // create new environment, insert loop variable
  auto env = NewEnv();
  auto loop_var = module_.CreateAlloca(ast.id_type());
  vals_->AddItem(ast.id(), loop_var);
  // create basic blocks
  const auto &func = module_.GetInsertPoint()->parent();
  auto cond_block = module_.CreateBlock(func, "for_cond");
  auto body_block = module_.CreateBlock(func, "for_body");
  auto end_block = module_.CreateBlock(func, "for_end");
  // add to break/continue stack
  break_cont_.push({end_block, cond_block});
  // generate expression
  const auto &expr_ty = ast.expr()->ast_type();
  auto expr_ptr = module_.CreateAlloca(expr_ty);
  auto expr_val = ast.expr()->GenerateIR(*this);
  module_.CreateStore(expr_val, expr_ptr);
  expr_val = module_.CreateLoad(expr_ptr, expr_ty->IsReference());
  module_.CreateJump(cond_block);
  // emit 'cond' block
  module_.SetInsertPoint(cond_block);
  auto last_val = module_.CreateCall(last_func, {expr_val});
  module_.CreateBranch(last_val, end_block, body_block);
  // emit 'body' block
  module_.SetInsertPoint(body_block);
  auto new_val = module_.CreateCall(next_func, {expr_val});
  module_.CreateInit(new_val, loop_var, ast.id_type()->IsReference());
  ast.body()->GenerateIR(*this);
  module_.CreateJump(cond_block);
  // emit 'end' block
  module_.SetInsertPoint(end_block);
  // pop the top element of break/continue stack
  break_cont_.pop();
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(AsmAST &ast) {
  auto context = module_.SetContext(ast.logger());
  module_.CreateAsm(ast.asm_str());
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(ControlAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // create basic block
  const auto &func = module_.GetInsertPoint()->parent();
  auto block = module_.CreateBlock(func);
  switch (ast.type()) {
    case Keyword::Break: case Keyword::Continue: {
      // generate target
      const auto &cur = break_cont_.top();
      auto target = ast.type() == Keyword::Break ? cur.first : cur.second;
      // generate jump
      module_.CreateJump(target);
      break;
    }
    case Keyword::Return: {
      // generate return value
      if (ast.expr()) {
        auto val = ast.expr()->GenerateIR(*this);
        module_.CreateInit(val, ret_val_, ret_is_ref_);
      }
      // generate jump
      module_.CreateJump(func_exit_);
      break;
    }
    default: assert(false); break;
  }
  // emit new block
  module_.SetInsertPoint(block);
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(WhenElemAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // create basic blocks
  const auto &func = module_.GetInsertPoint()->parent();
  auto body_block = module_.CreateBlock(func, "case_body");
  auto exit_block = module_.CreateBlock(func, "case_exit");
  // generate conditions
  for (const auto &i : ast.conds()) {
    // generate comparison
    auto rhs = i->GenerateIR(*this);
    auto eq = module_.CreateEqual(when_expr_, rhs);
    // generate branch
    auto next_block = module_.CreateBlock(func);
    module_.CreateBranch(eq, body_block, next_block);
    module_.SetInsertPoint(next_block);
  }
  // create branch to exit block
  module_.CreateJump(exit_block);
  // generate body
  module_.SetInsertPoint(body_block);
  auto ret = ast.body()->GenerateIR(*this);
  if (when_val_) module_.CreateInit(ret, when_val_, is_when_val_ref_);
  module_.CreateJump(when_end_);
  // emit 'exit' block
  module_.SetInsertPoint(exit_block);
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(BinaryAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // generate lhs
  auto lhs = ast.lhs()->GenerateIR(*this);
  // get name of overloaded function
  auto op_func = ast.op_func_id();
  // check if is logic operator (perform short circuit)
  if (!op_func &&
      (ast.op() == Operator::LogicAnd || ast.op() == Operator::LogicOr)) {
    // create basic blocks
    const auto &func = module_.GetInsertPoint()->parent();
    auto rhs_block = module_.CreateBlock(func, "logic_rhs");
    auto end_block = module_.CreateBlock(func, "logic_end");
    // generate result value
    assert(ast.ast_type()->IsBool() && !ast.ast_type()->IsReference());
    auto result = module_.CreateAlloca(ast.ast_type());
    // handle by operator
    if (ast.op() == Operator::LogicAnd) {
      module_.CreateStore(module_.GetBool(false), result);
      module_.CreateBranch(lhs, rhs_block, end_block);
    }
    else {  // LogicOr
      module_.CreateStore(module_.GetBool(true), result);
      module_.CreateBranch(lhs, end_block, rhs_block);
    }
    // emit 'rhs' block
    module_.SetInsertPoint(rhs_block);
    auto rhs = ast.rhs()->GenerateIR(*this);
    module_.CreateStore(rhs, result);
    module_.CreateJump(end_block);
    // emit 'end' block
    module_.SetInsertPoint(end_block);
    return module_.CreateLoad(result, false);
  }
  // generate rhs
  auto rhs = ast.rhs()->GenerateIR(*this);
  // try to handle operator overloading
  if (op_func) {
    // get function
    auto callee = vals_->GetItem(*op_func);
    // generate function call
    return module_.CreateCall(callee, {lhs, rhs});
  }
  // normal binary operation
  return CreateBinOp(ast.op(), lhs, rhs);
}

SSAPtr IRBuilder::GenerateOn(AccessAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // generate expression
  auto expr = ast.expr()->GenerateIR(*this);
  const auto &expr_ty = ast.expr()->ast_type();
  assert(expr_ty->IsStruct());
  // get index of element
  auto index = expr_ty->GetElemIndex(ast.id());
  assert(index);
  // generate access operation
  auto elem_ty = expr_ty->GetElem(*index);
  auto index_val = module_.GetInt32(*index);
  auto ptr = module_.CreateElemAccess(expr, index_val, elem_ty);
  // generate load
  return module_.CreateLoad(ptr, elem_ty->IsReference());
}

SSAPtr IRBuilder::GenerateOn(CastAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // generate expression
  auto expr = ast.expr()->GenerateIR(*this);
  // create type casting
  return module_.CreateCast(expr, ast.type()->ast_type());
}

SSAPtr IRBuilder::GenerateOn(UnaryAST &ast) {
  using UnaryOp = UnaryAST::UnaryOp;
  auto context = module_.SetContext(ast.logger());
  // generate operand
  auto opr = ast.opr()->GenerateIR(*this);
  assert(!ast.opr()->ast_type()->IsReference());
  // try to handle operator overloading
  auto op_func = ast.op_func_id();
  if (op_func) {
    // get function
    auto callee = vals_->GetItem(*op_func);
    // generate function call
    return module_.CreateCall(callee, {opr});
  }
  // normal unary operation
  switch (ast.op()) {
    case UnaryOp::Pos: return opr;
    case UnaryOp::Neg: return module_.CreateNeg(opr);
    case UnaryOp::LogicNot: return module_.CreateLogicNot(opr);
    case UnaryOp::Not: return module_.CreateNot(opr);
    case UnaryOp::DeRef: return module_.CreateLoad(opr, false);
    case UnaryOp::AddrOf: return opr->GetAddr();
    default: assert(false); return nullptr;
  }
}

SSAPtr IRBuilder::GenerateOn(IndexAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // generate expression & index
  auto expr = ast.expr()->GenerateIR(*this);
  auto index = ast.index()->GenerateIR(*this);
  // get type of expression
  auto expr_ty = ast.expr()->ast_type();
  if (expr_ty->IsReference()) expr_ty = expr_ty->GetDerefedType();
  auto elem_ty = expr_ty->GetDerefedType();
  // generate indexing operation
  SSAPtr ptr;
  if (expr_ty->IsArray()) {
    ptr = module_.CreateElemAccess(expr, index, elem_ty);
  }
  else {
    ptr = module_.CreatePtrAccess(expr, index);
  }
  // generate load
  return module_.CreateLoad(ptr, elem_ty->IsReference());
}

SSAPtr IRBuilder::GenerateOn(FunCallAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // generate callee
  auto callee = ast.expr()->GenerateIR(*this);
  // generate arguments
  SSAPtrList args;
  for (const auto &i : ast.args()) {
    args.push_back(i->GenerateIR(*this));
  }
  // generate function call
  return module_.CreateCall(callee, args);
}

SSAPtr IRBuilder::GenerateOn(IntAST &ast) {
  auto context = module_.SetContext(ast.logger());
  return module_.GetInt(ast.value(), ast.ast_type());
}

SSAPtr IRBuilder::GenerateOn(FloatAST &ast) {
  auto context = module_.SetContext(ast.logger());
  return module_.GetFloat(ast.value(), ast.ast_type());
}

SSAPtr IRBuilder::GenerateOn(CharAST &ast) {
  auto context = module_.SetContext(ast.logger());
  return module_.GetInt(ast.c(), ast.ast_type());
}

SSAPtr IRBuilder::GenerateOn(IdAST &ast) {
  auto context = module_.SetContext(ast.logger());
  // get value
  auto val = vals_->GetItem(ast.id());
  if (!val->type()->IsFunction()) {
    val = module_.CreateLoad(val, ast.ast_type()->IsReference());
  }
  return val;
}

SSAPtr IRBuilder::GenerateOn(StringAST &ast) {
  auto context = module_.SetContext(ast.logger());
  return module_.GetString(ast.str(), ast.ast_type());
}

SSAPtr IRBuilder::GenerateOn(BoolAST &ast) {
  auto context = module_.SetContext(ast.logger());
  return module_.GetBool(ast.value());
}

SSAPtr IRBuilder::GenerateOn(NullAST &ast) {
  auto context = module_.SetContext(ast.logger());
  return module_.GetZero(ast.ast_type());
}

SSAPtr IRBuilder::GenerateOn(ValInitAST &ast) {
  auto context = module_.SetContext(ast.logger());
  const auto &type = ast.type()->ast_type();
  if (ast.IsLiteral()) {
    // generate all elements
    SSAPtrList elems;
    for (int i = 0; i < type->GetLength(); ++i) {
      auto e = i < ast.elems().size() ? ast.elems()[i]->GenerateIR(*this)
                                      : module_.GetZero(type->GetElem(i));
      elems.push_back(std::move(e));
    }
    if (type->IsArray()) {
      // generate constant array
      return module_.GetArray(elems, type);
    }
    else {
      assert(type->IsStruct());
      // generate constant structure
      return module_.GetStruct(elems, type);
    }
  }
  else {
    // create a temporary alloca
    auto val = module_.CreateAlloca(type);
    assert(!type->IsReference());
    // generate zero initializer
    auto zero = module_.GetZero(type);
    module_.CreateStore(zero, val);
    // generate elements
    for (int i = 0; i < ast.elems().size(); ++i) {
      auto elem = ast.elems()[i]->GenerateIR(*this);
      const auto &ty = ast.elems()[i]->ast_type();
      auto ptr = module_.CreateElemAccess(val, module_.GetInt32(i), ty);
      module_.CreateStore(elem, ptr);
    }
    // generate load
    return module_.CreateLoad(val, false);
  }
}

SSAPtr IRBuilder::GenerateOn(PrimTypeAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(UserTypeAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(FuncTypeAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(VolaTypeAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(ArrayTypeAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(PointerTypeAST &ast) {
  // do nothing
  return nullptr;
}

SSAPtr IRBuilder::GenerateOn(RefTypeAST &ast) {
  // do nothing
  return nullptr;
}
