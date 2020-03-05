#include "front/analyzer.h"

#include <sstream>
#include <cmath>
#include <cassert>

using namespace yulang::front;
using namespace yulang::define;

namespace {

// table of operator's name
const char *kOperators[] = {YULANG_OPERATORS(YULANG_EXPAND_SECOND)};

// table of unary operator's name
const char *kUnaOperators[] = {"+", "-", "!", "~", "*", "&"};

// print error message
inline TypePtr LogError(const Logger &log, std::string_view message) {
  log.LogError(message);
  return nullptr;
}

// print error message (with specific identifier)
inline TypePtr LogError(const Logger &log, std::string_view message,
                        std::string_view id) {
  log.LogError(message, id);
  return nullptr;
}

}  // namespace

xstl::Guard Analyzer::NewEnv() {
  symbols_ = MakeEnv(symbols_);
  user_types_ = MakeEnv(user_types_);
  return xstl::Guard([this] {
    symbols_ = symbols_->outer();
    user_types_ = user_types_->outer();
  });
}

xstl::Guard Analyzer::EnterFunc(const TypePtr &ret) {
  cur_ret_ = ret;
  return xstl::Guard([this] { cur_ret_ = nullptr; });
}

std::string Analyzer::MangleFuncName(const std::string &id,
                                     const TypePtrList &args) {
  std::ostringstream oss;
  oss << "_$" << id << '_';
  for (const auto &i : args) {
    oss << i->GetTypeId();
  }
  return oss.str();
}

bool Analyzer::AddUserType(const Logger &log, const std::string &id,
                           TypePtr type) {
  if (user_types_->GetItem(id, false)) {
    LogError(log, "type has already been defined", id);
    return false;
  }
  user_types_->AddItem(id, std::move(type));
  return true;
}

bool Analyzer::AddVarConst(const Logger &log, const std::string &id,
                           TypePtr type, TypePtr init, bool is_var) {
  // check if is defined
  if (symbols_->GetItem(id, false)) {
    LogError(log, "identifier has already been defined", id);
    return false;
  }
  // check types
  TypePtr sym_type;
  if (type) {
    assert(!type->IsRightValue());
    // check if is compatible
    if (init && !type->CanAccept(init)) {
      LogError(log, "type mismatch when initializing", id);
      return false;
    }
    // check for reference types
    if (type->IsReference()) {
      if (!init) {
        LogError(log, "cannot define a reference "
                 "without initialization", id);
      }
      else if (init->IsRightValue()) {
        LogError(log, "reference cannot be initialized "
                 "with a right value", id);
      }
    }
    sym_type = std::move(type);
  }
  else {
    assert(init);
    // check if can be deduced
    if (init->IsVoid()) {
      LogError(log, "initializing with invalid type", id);
      return false;
    }
    // cast to left value type
    sym_type = std::move(init);
    if (sym_type->IsRightValue()) sym_type->GetValueType(false);
  }
  // add symbol info
  if (is_var && !sym_type->IsConst()) {
    sym_type = std::make_shared<ConstType>(std::move(sym_type));
  }
  symbols_->AddItem(id, std::move(sym_type));
  return true;
}

TypePtr Analyzer::FindFuncType(const Logger &log, const std::string &id,
                               const TypePtrList &args,
                               IdSetter id_setter) {
  auto mangled = MangleFuncName(id, args);
  auto type = symbols_->GetItem(mangled);
  if (!type) {
    type = symbols_->GetItem(id);
    if (!type || !type->IsFunction()) {
      return LogError(log, "function not found", id);
    }
    if (id_setter) id_setter(id);
  }
  else {
    if (id_setter) id_setter(mangled);
  }
  assert(type->IsFunction());
  return type;
}

std::optional<TypePtr> Analyzer::CheckOpOverload(
    const Logger &log, const std::string &op_name,
    const TypePtrList &args, IdSetter id_setter) {
  auto mangled = MangleFuncName(op_name, args);
  auto func_type = symbols_->GetItem(mangled);
  if (!func_type) {
    func_type = symbols_->GetItem(op_name);
    if (func_type && func_type->IsFunction()) {
      id_setter(op_name);
    }
    else {
      // no overloaded operator found
      return {};
    }
  }
  else {
    id_setter(mangled);
  }
  // check if is valid operator overloading
  if (func_type) {
    auto func_ret = func_type->GetReturnType(args);
    if (!func_ret) {
      return LogError(log, "invalid operator overloading");
    }
    return std::move(func_ret);
  }
  return {};
}

TypePtr Analyzer::AnalyzeOn(PropertyAST &ast) {
  // NOTE: this AST will not always be analyzed, so 'ast_type' may be null
  last_prop_ = ast.prop();
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(VarLetDefAST &ast) {
  for (const auto &i : ast.defs()) {
    if (!i->SemaAnalyze(*this)) return nullptr;
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(FunDefAST &ast) {
  // get types of arguments
  auto args_env = NewEnv();
  TypePtrList args;
  for (const auto &i : ast.args()) {
    auto type = i->SemaAnalyze(*this);
    if (!type) return nullptr;
    args.push_back(std::move(type));
  }
  // get return type
  auto ret = ast.type() ? ast.type()->SemaAnalyze(*this) : MakeVoid();
  if (!ret) return nullptr;
  auto func_guard = EnterFunc(ret);
  if (!ret->IsRightValue()) ret = ret->GetValueType(true);
  // perform function name mangling
  auto id = ast.id();
  if (id == ".") {
    return LogError(ast.logger(), "access operator cannot be overloaded");
  }
  ast.prop()->SemaAnalyze(*this);
  if (last_prop_ != PropertyAST::Property::Extern ||
      last_prop_ != PropertyAST::Property::Demangle) {
    id = MangleFuncName(id, args);
    ast.set_id(id);
  }
  // check if is existed
  // NOTE: current environment is argument env
  if (symbols_->outer()->GetItem(id, false)) {
    return LogError(ast.logger(), "function has already been defined", id);
  }
  // add function type info to environment
  auto type = std::make_shared<FuncType>(std::move(args), std::move(ret));
  symbols_->outer()->AddItem(id, std::move(type));
  // analyze function's body
  if (ast.body()) if (!ast.body()->SemaAnalyze(*this)) return nullptr;
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(DeclareAST &ast) {
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  // check if needs to perform name mangling
  auto id = ast.id();
  ast.prop()->SemaAnalyze(*this);
  if (type->IsFunction() &&
      (last_prop_ != PropertyAST::Property::Extern ||
       last_prop_ != PropertyAST::Property::Demangle)) {
    id = MangleFuncName(id, *type->GetArgsType());
    ast.set_id(id);
  }
  // add type info to environment
  if (symbols_->GetItem(id, false)) {
    return LogError(ast.logger(), "symbol has already been defined", id);
  }
  symbols_->AddItem(id, std::move(type));
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(TypeAliasAST &ast) {
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  // add type alias to environment
  if (!AddUserType(ast.logger(), ast.id(), std::move(type))) {
    return nullptr;
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(StructAST &ast) {
  // analyze elements
  TypePairList elems;
  for (const auto &i : ast.defs()) {
    if (!i->SemaAnalyze(*this)) return nullptr;
    elems.push_back(std::move(last_struct_info_));
  }
  // add user type to environment
  auto type = std::make_shared<SturctType>(std::move(elems));
  if (!AddUserType(ast.logger(), ast.id(), std::move(type))) {
    return nullptr;
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(EnumAST &ast) {
  // get type
  auto type = ast.type() ? ast.type()->SemaAnalyze(*this)
                         : MakePrimType(Keyword::Int32, false);
  if (!type) return nullptr;
  last_enum_type_ = type;
  // get all elements
  EnumType::ElemSet elems;
  for (const auto &i : ast.defs()) {
    if (!i->SemaAnalyze(*this)) return nullptr;
    auto [_, succ] = elems.insert(last_enum_elem_name_);
    if (!succ) {
      return LogError(i->logger(), "conflicted enumeration element name",
                      last_enum_elem_name_);
    }
  }
  // add user type to environment
  auto enum_type = std::make_shared<EnumType>(std::move(type),
                                              std::move(elems));
  if (!AddUserType(ast.logger(), ast.id(), std::move(enum_type))) {
    return nullptr;
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(ImportAST &ast) {
  for (const auto &i : ast.defs()) {
    if (!i->SemaAnalyze(*this)) return nullptr;
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(VarElemAST &ast) {
  // try to get variable type and initializer type
  TypePtr type, init;
  if (ast.type()) {
    type = ast.type()->SemaAnalyze(*this);
    if (!type) return nullptr;
  }
  if (ast.init()) {
    init = ast.init()->SemaAnalyze(*this);
    if (!init) return nullptr;
  }
  // add symbol to environment
  if (!AddVarConst(ast.logger(), ast.id(), std::move(type),
                   std::move(init), true)) {
    return nullptr;
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(LetElemAST &ast) {
  // try to get constant type and initializer type
  TypePtr type, init;
  if (ast.type()) {
    type = ast.type()->SemaAnalyze(*this);
    if (!type) return nullptr;
  }
  if (ast.init()) {
    init = ast.init()->SemaAnalyze(*this);
    if (!init) return nullptr;
  }
  // add symbol to environment
  if (!AddVarConst(ast.logger(), ast.id(), std::move(type),
                   std::move(init), true)) {
    return nullptr;
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(ArgElemAST &ast) {
  // get type
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  // add symbol to environment
  assert(!type->IsConst());
  type = std::make_shared<ConstType>(std::move(type));
  symbols_->AddItem(ast.id(), type);
  return ast.set_ast_type(std::move(type));
}

TypePtr Analyzer::AnalyzeOn(StructElemAST &ast) {
  // get type
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  // set last argument info (for structures)
  assert(!type->IsRightValue());
  if (type->IsReference()) {
    return LogError(ast.logger(),
                    "type of structure element cannot be reference");
  }
  last_struct_info_ = {ast.id(), type};
  return ast.set_ast_type(std::move(type));
}

TypePtr Analyzer::AnalyzeOn(EnumElemAST &ast) {
  // check type of initialization expression
  if (ast.expr()) {
    auto type = ast.expr()->SemaAnalyze(*this);
    if (!type) return nullptr;
    if (!last_enum_type_->CanAccept(type)) {
      return LogError(ast.logger(), "invalid initialization expression");
    }
  }
  // update enum info
  last_enum_elem_name_ = ast.id();
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(BlockAST &ast) {
  auto env = NewEnv();
  auto ret = MakeVoid();
  for (int i = 0; i < ast.stmts().size(); ++i) {
    auto type = ast.stmts()[i]->SemaAnalyze(*this);
    if (!type) return nullptr;
    if (i == ast.stmts().size() - 1) ret = std::move(type);
  }
  if (!ret->IsRightValue()) ret = ret->GetValueType(true);
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(IfAST &ast) {
  // check condition
  auto cond = ast.cond()->SemaAnalyze(*this);
  if (!cond) return nullptr;
  if (!cond->IsBool()) {
    return LogError(ast.cond()->logger(), "condition must be a boolean");
  }
  // check then block
  auto then = ast.then()->SemaAnalyze(*this);
  if (!then) return nullptr;
  // check else then block
  auto else_then = ast.else_then() ? ast.else_then()->SemaAnalyze(*this)
                                   : MakeVoid();
  if (!else_then) return nullptr;
  // create return type
  auto ret = then->IsIdentical(else_then) ? then : MakeVoid();
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(WhenAST &ast) {
  // get type of expression
  last_when_expr_type_ = ast.expr()->SemaAnalyze(*this);
  if (!last_when_expr_type_) return nullptr;
  // check all elements
  TypePtr elems;
  for (const auto &i : ast.elems()) {
    auto elem = i->SemaAnalyze(*this);
    if (!elem) return nullptr;
    // try to determine return type
    if (!elems) {
      elems = std::move(elem);
    }
    else if (!elems->IsVoid() && !elems->IsIdentical(elem)) {
      elems = MakeVoid();
    }
  }
  // check else then block
  auto else_then = ast.else_then() ? ast.else_then()->SemaAnalyze(*this)
                                   : MakeVoid();
  if (!else_then) return nullptr;
  // create return type
  auto ret = elems->IsIdentical(else_then) ? elems : MakeVoid();
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(WhileAST &ast) {
  // check condition
  auto cond = ast.cond()->SemaAnalyze(*this);
  if (!cond) return nullptr;
  if (!cond->IsBool()) {
    return LogError(ast.cond()->logger(), "condition must be a boolean");
  }
  // check body
  ++in_loop_;
  if (!ast.body()->SemaAnalyze(*this)) return nullptr;
  --in_loop_;
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(ForInAST &ast) {
  auto env = NewEnv();
  // get type of expression
  auto expr = ast.expr()->SemaAnalyze(*this);
  if (!expr) return nullptr;
  // find iterator
  TypePtrList args = {std::move(expr)};
  auto next =
      FindFuncType(ast.expr()->logger(), "next", args,
                   [&ast](const std::string &id) { ast.set_next_id(id); });
  auto last =
      FindFuncType(ast.expr()->logger(), "last", args,
                   [&ast](const std::string &id) { ast.set_last_id(id); });
  if (!next || !last) return nullptr;
  // check iterator
  auto type = next->GetReturnType(args);
  if (type->IsVoid() || !last->GetReturnType(args)->IsBool()) {
    return LogError(ast.expr()->logger(), "invalid iterator");
  }
  // create loop variable
  if (!type->IsConst()) {
    type = std::make_shared<ConstType>(std::move(type));
  }
  symbols_->AddItem(ast.id(), type);
  // check body
  ++in_loop_;
  if (!ast.body()->SemaAnalyze(*this)) return nullptr;
  --in_loop_;
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(AsmAST &ast) {
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(ControlAST &ast) {
  switch (ast.type()) {
    case Keyword::Break: case Keyword::Continue: {
      // check if is in a loop
      if (!in_loop_) {
        return LogError(ast.logger(),
                        "using break/continue outside the loop");
      }
      break;
    }
    case Keyword::Return: {
      // check if is in a function
      if (!cur_ret_) {
        return LogError(ast.logger(),
                        "using 'return' outside the function");
      }
      else {
        auto type = ast.expr() ? ast.expr()->SemaAnalyze(*this)
                               : MakeVoid();
        // check if is compatible
        assert(cur_ret_->IsVoid() || !cur_ret_->IsRightValue());
        if (!cur_ret_->CanAccept(type)) {
          return LogError(ast.expr()->logger(),
                          "type mismatch when returning");
        }
      }
      break;
    }
    default: assert(false);
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(WhenElemAST &ast) {
  // check conditions
  for (const auto &i : ast.conds()) {
    auto cond = i->SemaAnalyze(*this);
    if (!cond) return nullptr;
    if (!cond->IsIdentical(last_when_expr_type_)) {
      return LogError(i->logger(), "condition type mismatch");
    }
  }
  // check body
  auto body = ast.body()->SemaAnalyze(*this);
  if (!body) return nullptr;
  return ast.set_ast_type(std::move(body));
}

TypePtr Analyzer::AnalyzeOn(BinaryAST &ast) {
  // get lhs and rhs type
  auto lhs = ast.lhs()->SemaAnalyze(*this);
  auto rhs = ast.lhs()->SemaAnalyze(*this);
  if (!lhs || !rhs) {
    return LogError(ast.logger(), "invalid lhs or rhs expression");
  }
  // preprocess some types
  if (lhs->IsVoid() || rhs->IsVoid()) {
    return LogError(ast.logger(), "invalid operation between void types");
  }
  if (lhs->IsReference()) lhs = lhs->GetDerefedType();
  if (rhs->IsReference()) rhs = rhs->GetDerefedType();
  // check if operator was overloaded
  if (!lhs->IsBasic() || !rhs->IsBasic()) {
    TypePtrList args = {lhs, rhs};
    auto op_name = kOperators[static_cast<int>(ast.op())];
    auto ret = CheckOpOverload(
        ast.logger(), op_name, args,
        [&ast](const std::string &id) { ast.set_op_func_id(id); });
    if (ret) return ast.set_ast_type(*ret);
  }
  // normal binary operation
  TypePtr ret;
  switch (ast.op()) {
    case Operator::Add: case Operator::Sub: {
      if (lhs->IsPointer() || rhs->IsPointer()) {
        // pointer operation
        if (lhs->IsPointer() && rhs->IsInteger()) {
          ret = lhs;
        }
        else if (rhs->IsPointer() && lhs->IsInteger()) {
          ret = rhs;
        }
        else {
          return LogError(ast.logger(), "invalid pointer operation");
        }
      }
      // fall through
    }
    case Operator::Mul: case Operator::Div: case Operator::Mod: {
      // float binary operation
      if (lhs->IsFloat() && lhs->IsIdentical(rhs)) ret = lhs;
      // fall through
    }
    case Operator::And: case Operator::Or: case Operator::Xor:
    case Operator::Shl: case Operator::Shr: {
      // int binary operation
      if (lhs->IsInteger() && lhs->IsIdentical(rhs)) ret = lhs;
      break;
    }
    case Operator::Less: case Operator::LessEqual:
    case Operator::Great: case Operator::GreatEqual: {
      // int/float binary operation
      if ((lhs->IsInteger() || lhs->IsFloat()) && lhs->IsIdentical(rhs)) {
        ret = MakePrimType(Keyword::Bool, true);
      }
      break;
    }
    case Operator::LogicAnd: case Operator::LogicOr: {
      // bool binary operation
      if (lhs->IsBool() && lhs->IsIdentical(rhs)) ret = lhs;
      break;
    }
    case Operator::Equal: case Operator::NotEqual: {
      // binary operation between all types
      if (lhs->IsIdentical(rhs)) ret = MakePrimType(Keyword::Bool, true);
      break;
    }
    case Operator::Assign: {
      // binary operation between all types
      if (!lhs->IsRightValue() && lhs->IsIdentical(rhs)) ret = MakeVoid();
      break;
    }
    case Operator::AssAdd: case Operator::AssSub: {
      // pointer operation
      if (lhs->IsPointer() && rhs->IsInteger()) ret = MakeVoid();
      // fall through
    }
    case Operator::AssMul: case Operator::AssDiv: case Operator::AssMod: {
      // float binary operation
      if (lhs->IsFloat() && lhs->IsIdentical(rhs)) ret = MakeVoid();
      // fall through
    }
    case Operator::AssAnd: case Operator::AssOr: case Operator::AssXor:
    case Operator::AssShl: case Operator::AssShr: {
      // int binary operation
      if (lhs->IsRightValue()) {
        ret = nullptr;
      }
      else if (lhs->IsInteger() && lhs->IsIdentical(rhs)) {
        ret = MakeVoid();
      }
      break;
    }
    default: assert(false); return nullptr;
  }
  if (!ret) return LogError(ast.logger(), "invalid binary operation");
  if (!ret->IsRightValue()) ret = ret->GetValueType(true);
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(AccessAST &ast) {
  // get expression type & id
  last_id_ = {};
  auto expr = ast.expr()->SemaAnalyze(*this);
  auto expr_id = last_id_;
  last_id_ = {};
  // handle access
  TypePtr ret;
  if (!expr && expr_id) {
    // check if is enumeration access
    auto enum_type = user_types_->GetItem(*expr_id);
    if (enum_type->IsEnum() && enum_type->GetElem(ast.id())) {
      ret = std::move(enum_type);
    }
  }
  else if (expr && expr->GetLength()) {
    // check if is structure access
    auto type = expr->GetElem(ast.id());
    if (type) ret = std::move(type);
  }
  // check if is error
  if (!ret) {
    return LogError(ast.logger(), "invalid access operation");
  }
  // get return type
  ret = ret->IsRightValue() ? ret : ret->GetValueType(true);
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(CastAST &ast) {
  auto expr = ast.expr()->SemaAnalyze(*this);
  auto type = ast.type()->SemaAnalyze(*this);
  if (!expr || !type) return nullptr;
  // check if is const cast
  auto expr_deref = expr->IsReference() ? expr->GetDerefedType() : expr;
  if (expr_deref->IsConst() && !type->IsConst()) {
    return LogError(ast.logger(), "const cast is not allowed");
  }
  // check if cast is valid
  if (type->IsReference() || !expr->CanCastTo(type)) {
    return LogError(ast.logger(), "invalid type casting");
  }
  return ast.set_ast_type(type->GetValueType(true));
}

TypePtr Analyzer::AnalyzeOn(UnaryAST &ast) {
  using UnaryOp = UnaryAST::UnaryOp;
  // get lhs and rhs type & id
  auto opr = ast.opr()->SemaAnalyze(*this);
  if (!opr || opr->IsVoid()) {
    return LogError(ast.opr()->logger(), "invalid operand");
  }
  if (opr->IsReference()) opr = opr->GetDerefedType();
  // check if operator was overloaded
  // NOTE: 'sizeof' cannot be overloaded
  if (!opr->IsBasic() && ast.op() != UnaryOp::SizeOf) {
    TypePtrList args = {opr};
    auto op_name = kUnaOperators[static_cast<int>(ast.op())];
    auto ret = CheckOpOverload(
        ast.logger(), op_name, args,
        [&ast](const std::string &id) { ast.set_op_func_id(id); });
    if (ret) return ast.set_ast_type(*ret);
  }
  // normal unary operations
  TypePtr ret;
  switch (ast.op()) {
    case UnaryOp::Pos: case UnaryOp::Neg: {
      // int/float unary operation
      if (opr->IsInteger() || opr->IsFloat()) ret = opr;
      break;
    }
    case UnaryOp::LogicNot: {
      // int/bool unary operation
      if (opr->IsInteger() || opr->IsBool()) ret = opr;
      break;
    }
    case UnaryOp::Not: {
      // int unary operation
      if (opr->IsInteger()) ret = opr;
      break;
    }
    case UnaryOp::DeRef: {
      // pointer unary operation
      if (opr->IsPointer()) ret = opr->GetDerefedType();
      break;
    }
    case UnaryOp::AddrOf: {
      // left value unary operation
      if (!opr->IsRightValue()) {
        ret = std::make_shared<PointerType>(opr);
      }
      break;
    }
    case UnaryOp::SizeOf: {
      ret = MakePrimType(Keyword::UInt32, true);
      break;
    }
    default: assert(false); return nullptr;
  }
  if (!ret) return LogError(ast.logger(), "invalid unary operation");
  if (!ret->IsRightValue()) ret = ret->GetValueType(true);
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(IndexAST &ast) {
  // get type of expression
  auto expr = ast.expr()->SemaAnalyze(*this);
  if (!expr || !expr->IsPointer() || !expr->IsArray()) {
    return LogError(ast.expr()->logger(),
                    "expression is not subscriptable");
  }
  // get type of index
  auto index = ast.index()->SemaAnalyze(*this);
  if (!index->IsInteger()) {
    return LogError(ast.index()->logger(), "invalid index");
  }
  // get return type
  auto ret = expr->GetDerefedType();
  if (expr->IsArray()) {
    auto val = ast.index()->Eval(eval_);
    if (val) {
      // check if out of bounds
      auto val_ptr = std::get_if<std::uint64_t>(&*val);
      assert(val_ptr);
      if (*val_ptr >= expr->GetLength()) {
        ast.index()->logger().LogWarning("subscript out of bounds");
      }
    }
  }
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(FunCallAST &ast) {
  // get type of arguments
  TypePtrList args;
  for (const auto &i : ast.args()) {
    auto arg = i->SemaAnalyze(*this);
    if (!arg) return nullptr;
    args.push_back(std::move(arg));
  }
  // get function type
  last_id_ = {};
  auto type = ast.expr()->SemaAnalyze(*this);
  if (last_id_) {
    auto id_setter = [&ast](const std::string &id) {
      static_cast<const IdAST *>(ast.expr().get())->set_id(id);
    };
    type = FindFuncType(ast.expr()->logger(), *last_id_, args, id_setter);
    last_id_ = {};
  }
  // check function type
  if (!type || !type->IsFunction()) {
    return LogError(ast.expr()->logger(), "calling a non-function");
  }
  // get return type
  auto ret = type->GetReturnType(args);
  if (!ret) {
    return LogError(ast.expr()->logger(), "invalid function call");
  }
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(IntAST &ast) {
  return ast.set_ast_type(MakePrimType(Keyword::Int32, true));
}

TypePtr Analyzer::AnalyzeOn(FloatAST &ast) {
  return ast.set_ast_type(MakePrimType(Keyword::Float64, true));
}

TypePtr Analyzer::AnalyzeOn(CharAST &ast) {
  return ast.set_ast_type(MakePrimType(Keyword::UInt8, true));
}

TypePtr Analyzer::AnalyzeOn(IdAST &ast) {
  last_id_ = ast.id();
  return ast.set_ast_type(symbols_->GetItem(ast.id()));
}

TypePtr Analyzer::AnalyzeOn(StringAST &ast) {
  auto u8t = MakePrimType(Keyword::UInt8, true);
  auto cu8t = std::make_shared<ConstType>(std::move(u8t));
  auto strt = std::make_shared<PointerType>(std::move(cu8t));
  return ast.set_ast_type(std::move(strt));
}

TypePtr Analyzer::AnalyzeOn(BoolAST &ast) {
  return ast.set_ast_type(MakePrimType(Keyword::Bool, true));
}

TypePtr Analyzer::AnalyzeOn(NullAST &ast) {
  return ast.set_ast_type(MakePrimType(Keyword::Null, true));
}

TypePtr Analyzer::AnalyzeOn(ValInitAST &ast) {
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  // create left value type of current type
  if (type->IsRightValue()) {
    type = type->GetValueType(false);
  }
  // check if is a valid initializer list
  if (type->GetLength() > ast.elems().size()) {
    return LogError(ast.logger(), "initializer list length exceeded");
  }
  for (int i = 0; i < type->GetLength(); ++i) {
    // get type of element
    auto elem = ast.elems()[i]->SemaAnalyze(*this);
    if (!elem) return nullptr;
    // check if can be accepted
    if (!type->GetElem(i)->CanAccept(elem)) {
      return LogError(ast.logger(), "invalid initializer list");
    }
  }
  return ast.set_ast_type(type->GetValueType(true));
}

TypePtr Analyzer::AnalyzeOn(PrimTypeAST &ast) {
  return ast.set_ast_type(MakePrimType(ast.type(), false));
}

TypePtr Analyzer::AnalyzeOn(UserTypeAST &ast) {
  auto type = user_types_->GetItem(ast.id());
  if (!type) {
    return LogError(ast.logger(), "type has not been defined", ast.id());
  }
  return ast.set_ast_type(std::move(type));
}

TypePtr Analyzer::AnalyzeOn(FuncTypeAST &ast) {
  // get type of arguments
  TypePtrList args;
  for (const auto &i : ast.args()) {
    auto arg = i->SemaAnalyze(*this);
    if (!arg) return nullptr;
    args.push_back(std::move(arg));
  }
  // get return type
  auto ret = ast.ret()->SemaAnalyze(*this);
  if (!ret) return nullptr;
  // make function type
  auto type = std::make_shared<FuncType>(std::move(args), std::move(ret));
  return ast.set_ast_type(std::move(type));
}

TypePtr Analyzer::AnalyzeOn(VolaTypeAST &ast) {
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  return ast.set_ast_type(std::make_shared<VolaType>(std::move(type)));
}

TypePtr Analyzer::AnalyzeOn(ArrayTypeAST &ast) {
  // get base type
  auto base = ast.base()->SemaAnalyze(*this);
  if (!base) return nullptr;
  // try to evaluate array length
  auto val = ast.expr()->Eval(eval_);
  if (!val) return LogError(ast.expr()->logger(), "invalid array length");
  auto len_ptr = std::get_if<std::uint64_t>(&*val);
  assert(len_ptr);
  // create array type
  auto arr = std::make_shared<ArrayType>(std::move(base), *len_ptr);
  return ast.set_ast_type(std::move(arr));
}

TypePtr Analyzer::AnalyzeOn(PointerTypeAST &ast) {
  // get base type
  auto base = ast.base()->SemaAnalyze(*this);
  if (!base) return nullptr;
  if (!ast.is_var()) base = std::make_shared<ConstType>(std::move(base));
  return ast.set_ast_type(std::make_shared<PointerType>(std::move(base)));
}

TypePtr Analyzer::AnalyzeOn(RefTypeAST &ast) {
  // get base type
  auto base = ast.base()->SemaAnalyze(*this);
  if (!base) return nullptr;
  if (!ast.is_var()) base = std::make_shared<ConstType>(std::move(base));
  return ast.set_ast_type(std::make_shared<RefType>(std::move(base)));
}
