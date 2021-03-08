#include "front/analyzer.h"

#include <sstream>
#include <unordered_set>
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

// print error message (with nullable identifier)
inline bool LogErrorBool(const Logger &log, std::string_view message,
                         std::string_view id) {
  if (id.empty()) {
    log.LogError(message);
  }
  else {
    log.LogError(message, id);
  }
  return false;
}

}  // namespace

xstl::Guard Analyzer::NewEnv() {
  symbols_ = MakeEnv(symbols_);
  user_types_ = MakeEnv(user_types_);
  funcs_ = MakeFuncMap(funcs_);
  return xstl::Guard([this] {
    symbols_ = symbols_->outer();
    user_types_ = user_types_->outer();
    funcs_ = funcs_->outer();
  });
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

bool Analyzer::CheckInit(const Logger &log, const TypePtr &type,
                         const TypePtr &init) {
  return CheckInit(log, type, init, "");
}

bool Analyzer::CheckInit(const Logger &log, const TypePtr &type,
                         const TypePtr &init, std::string_view id) {
  // check if is compatible
  if (!type->IsIdentical(init)) {
    return LogErrorBool(log, "type mismatch when initializing", id);
  }
  // check for reference types
  if (type->IsReference()) {
    if (!init->IsReference() && init->IsRightValue()) {
      return LogErrorBool(log, "reference cannot be initialized "
                          "with a right value", id);
    }
    if (init->IsConst() && !type->GetDerefedType()->IsConst()) {
      return LogErrorBool(log, "variable reference cannot be initialized "
                          "with a constant", id);
    }
  }
  return true;
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

TypePtr Analyzer::AnalyzeOn(VarLetDefAST &ast) {
  last_prop_ = ast.prop();
  for (const auto &i : ast.defs()) {
    if (!i->SemaAnalyze(*this)) return nullptr;
    // run evaluation on var/let definitions for constant definitions
    i->Eval(eval_);
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
  cur_ret_ = ret;
  if (!ret->IsReference() && !ret->IsRightValue()) {
    ret = ret->GetValueType(true);
  }
  // perform function name mangling
  auto id = ast.id(), org_id = id;
  if (id == ".") {
    return LogError(ast.logger(), "access operator cannot be overloaded");
  }
  if (ast.prop() != Property::Extern) {
    id = MangleFuncName(id, args);
    ast.set_id(id);
  }
  // check if is existed
  // NOTE: current environment is argument env
  if (symbols_->outer()->GetItem(id, false)) {
    return LogError(ast.logger(), "function has already been defined", id);
  }
  // add function type info to symbol environment
  auto type = std::make_shared<FuncType>(std::move(args), std::move(ret),
                                         true);
  symbols_->outer()->AddItem(id, type);
  // add function type info to function mapping table
  if (!funcs_->outer()->GetItem(org_id)) {
    funcs_->outer()->AddItem(org_id, id);
  }
  else {
    funcs_->outer()->RemoveItem(org_id);
  }
  // analyze function's body
  if (ast.body()) {
    auto body_ret = ast.body()->SemaAnalyze(*this);
    if (!body_ret) return nullptr;
    if (!cur_ret_->IsVoid() &&
        !CheckInit(ast.body()->logger(), cur_ret_, body_ret)) {
      return nullptr;
    }
  }
  ast.set_ast_type(std::move(type));
  return MakeVoid();
}

TypePtr Analyzer::AnalyzeOn(DeclareAST &ast) {
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  // preprocess type
  if (type->IsFunction()) {
    assert(!type->IsRightValue());
    type = type->GetValueType(true);
  }
  if (!ast.is_var() && !type->IsConst()) {
    type = std::make_shared<ConstType>(std::move(type));
    ast.type()->set_ast_type(type);
  }
  // check if needs to perform name mangling
  auto id = ast.id();
  if (type->IsFunction() && ast.prop() != Property::Extern) {
    id = MangleFuncName(id, *type->GetArgsType());
    ast.set_id(id);
  }
  // add type info to environment
  if (!symbols_->is_root() && !type->IsFunction()) {
    return LogError(ast.logger(), "cannot declare non-function "
                    "in function", id);
  }
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
  TypePairList elems;
  // create an empty struct type
  auto type = std::make_shared<StructType>(elems, ast.id(), false);
  if (!AddUserType(ast.logger(), ast.id(), type)) return nullptr;
  // analyze elements
  std::unordered_set<std::string_view> names;
  for (const auto &i : ast.defs()) {
    auto elem = i->SemaAnalyze(*this);
    if (!elem) return nullptr;
    // check if name conflicted
    auto [_, succ] = names.insert(last_struct_elem_name_);
    if (!succ) {
      return LogError(i->logger(), "conflicted struct element name",
                      last_struct_elem_name_);
    }
    // check if is recursive type
    auto ty = elem;
    while (ty->IsArray()) ty = ty->GetDerefedType();
    if (ty->IsStruct() && ty->GetTypeId() == ast.id()) {
      return LogError(i->logger(), "recursive type is not allowed",
                      last_struct_elem_name_);
    }
    elems.push_back({std::string(last_struct_elem_name_),
                     std::move(elem)});
  }
  // update struct type
  // TODO: circular reference!
  type->set_elems(std::move(elems));
  ast.set_ast_type(std::move(type));
  return MakeVoid();
}

TypePtr Analyzer::AnalyzeOn(EnumAST &ast) {
  // get type
  auto type = ast.type() ? ast.type()->SemaAnalyze(*this)
                         : MakePrimType(Keyword::Int32, false);
  if (!type) return nullptr;
  if (!type->IsInteger() || type->IsReference()) {
    return LogError(ast.logger(),
                    "enumuration's type must be an integer type");
  }
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
  auto enum_type = std::make_shared<EnumType>(
      std::move(type), std::move(elems), ast.id(), false);
  if (!AddUserType(ast.logger(), ast.id(), enum_type)) return nullptr;
  // add enumeration right value to symbol environment
  if (symbols_->GetItem(ast.id(), false)) {
    return LogError(ast.logger(), "enumeration name has already been used "
                    "by another symbol", ast.id());
  }
  symbols_->AddItem(ast.id(), enum_type->GetValueType(true));
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(ImportAST &ast) {
  for (const auto &i : ast.defs()) {
    if (!i->SemaAnalyze(*this)) return nullptr;
    // run evaluation on imported ASTs for constant definitions
    i->Eval(eval_);
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(VarLetElemAST &ast) {
  const auto &log = ast.logger();
  const auto &id = ast.id();
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
  // check if is ill-formed
  if (last_prop_ == Property::Public || last_prop_ == Property::Extern) {
    if (!type) {
      return LogError(log, "'public' definitions must "
                      "specify type explicitly");
    }
  }
  else if ((ast.is_var() && !type && !init) || (!ast.is_var() && !init)) {
    return LogError(log, "initializer required");
  }
  // check if is defined
  if (symbols_->GetItem(id, false)) {
    return LogError(log, "identifier has already been defined", id);
  }
  // check types
  TypePtr sym_type;
  if (type) {
    assert(!type->IsRightValue());
    if (init && !CheckInit(log, type, init, id)) return nullptr;
    // check for reference types
    if (!init && type->IsReference()) {
      return LogError(log, "cannot define a reference "
                      "without initialization", id);
    }
    sym_type = std::move(type);
  }
  else {
    // check if can be deduced
    if (init->IsVoid() || init->IsNull()) {
      return LogError(log, "initializing with invalid type", id);
    }
    // remove reference because the type is not specified explicitly
    if (init->IsReference()) init = init->GetDerefedType();
    // cast to left value type
    if (init->IsRightValue()) init = init->GetValueType(false);
    sym_type = std::move(init);
  }
  // add symbol info
  if (ast.is_var()) {
    if (sym_type->IsConst()) sym_type = sym_type->GetDeconstedType();
  }
  else if (!sym_type->IsConst()) {
    sym_type = std::make_shared<ConstType>(std::move(sym_type));
  }
  symbols_->AddItem(id, sym_type);
  ast.set_ast_type(std::move(sym_type));
  return MakeVoid();
}

TypePtr Analyzer::AnalyzeOn(ArgElemAST &ast) {
  // get type
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  // check if is conflicted
  if (symbols_->GetItem(ast.id(), false)) {
    return LogError(ast.logger(), "argument has already been declared",
                    ast.id());
  }
  // add symbol to environment
  assert(!type->IsConst() || type->IsReference());
  if (!type->IsReference()) {
    type = std::make_shared<ConstType>(std::move(type));
  }
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
  last_struct_elem_name_ = ast.id();
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
  return ast.set_ast_type(last_enum_type_);
}

TypePtr Analyzer::AnalyzeOn(BlockAST &ast) {
  auto env = NewEnv();
  auto ret = MakeVoid();
  for (std::size_t i = 0; i < ast.stmts().size(); ++i) {
    auto type = ast.stmts()[i]->SemaAnalyze(*this);
    if (!type) return nullptr;
    if (i == ast.stmts().size() - 1) ret = std::move(type);
  }
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
  auto ret = MakeVoid();
  if (then->IsIdentical(else_then)) {
    // if one of return value is right value, return right value
    ret = then->IsRightValue() ? std::move(then) : std::move(else_then);
  }
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
    else if (!elems->IsVoid()) {
      if (!elems->IsIdentical(elem)) {
        elems = MakeVoid();
      }
      else if (!elems->IsRightValue() && elem->IsRightValue()) {
        // handle right value, same as if-statement
        elems = std::move(elem);
      }
    }
  }
  // check else then block
  auto else_then = ast.else_then() ? ast.else_then()->SemaAnalyze(*this)
                                   : MakeVoid();
  if (!else_then) return nullptr;
  // create return type
  auto ret = MakeVoid();
  if (elems->IsIdentical(else_then)) {
    // handle right value, same as if-statement
    ret = elems->IsRightValue() ? std::move(elems) : std::move(else_then);
  }
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
  if (expr->IsRightValue()) expr = expr->GetValueType(false);
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
  if (type->IsRightValue()) type = type->GetValueType(false);
  if (!type->IsReference() && !type->IsConst()) {
    type = std::make_shared<ConstType>(std::move(type));
  }
  symbols_->AddItem(ast.id(), type);
  ast.set_id_type(std::move(type));
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
      auto type = ast.expr() ? ast.expr()->SemaAnalyze(*this) : MakeVoid();
      // check if is compatible
      assert(cur_ret_->IsVoid() || !cur_ret_->IsRightValue());
      if (!CheckInit(ast.logger(), cur_ret_, type)) return nullptr;
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
  auto rhs = ast.rhs()->SemaAnalyze(*this);
  if (!lhs) {
    return LogError(ast.lhs()->logger(), "invalid lhs expression");
  }
  if (!rhs) {
    return LogError(ast.rhs()->logger(), "invalid rhs expression");
  }
  // preprocess some types
  if (lhs->IsVoid() || rhs->IsVoid()) {
    return LogError(ast.logger(), "invalid operation between void types");
  }
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
        else if (rhs->IsPointer() && lhs->IsInteger() &&
                 ast.op() != Operator::Sub) {
          ret = rhs;
        }
        else {
          return LogError(ast.logger(), "invalid pointer operation");
        }
        break;
      }
      // fall through
    }
    case Operator::Mul: case Operator::Div: case Operator::Mod: {
      // float binary operation
      if (lhs->IsFloat() && lhs->IsIdentical(rhs)) {
        ret = lhs;
        break;
      }
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
      if ((lhs->IsInteger() || lhs->IsFloat() || lhs->IsPointer()) &&
          lhs->IsIdentical(rhs)) {
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
      // binary operation between all types except structures and arrays
      if (!lhs->IsStruct() && !lhs->IsArray() && lhs->IsIdentical(rhs)) {
        ret = MakePrimType(Keyword::Bool, true);
      }
      break;
    }
    case Operator::Assign: {
      // binary operation between all types
      if (lhs->CanAccept(rhs)) ret = MakeVoid();
      break;
    }
    case Operator::AssAdd: case Operator::AssSub: {
      // pointer operation
      if (lhs->IsPointer() && !lhs->IsRightValue() && !lhs->IsConst() &&
          rhs->IsInteger()) {
        ret = MakeVoid();
        break;
      }
      // fall through
    }
    case Operator::AssMul: case Operator::AssDiv: case Operator::AssMod: {
      // float binary operation
      if (lhs->IsFloat() && lhs->CanAccept(rhs)) {
        ret = MakeVoid();
        break;
      }
      // fall through
    }
    case Operator::AssAnd: case Operator::AssOr: case Operator::AssXor:
    case Operator::AssShl: case Operator::AssShr: {
      // int binary operation
      if (lhs->IsInteger() && lhs->CanAccept(rhs)) ret = MakeVoid();
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
  auto expr = ast.expr()->SemaAnalyze(*this);
  // handle access
  TypePtr ret;
  if (expr) {
    if (expr->IsEnum() && expr->GetElem(ast.id())) {
      // check if is enumeration access
      assert(expr->IsRightValue());
      ret = std::move(expr);
    }
    else if (expr->IsStruct() && !expr->IsRightValue()) {
      // check if is structure access
      auto type = expr->GetElem(ast.id());
      if (type) ret = std::move(type);
    }
  }
  // check if is error
  if (!ret) {
    return LogError(ast.logger(), "invalid access operation");
  }
  // get return type
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(CastAST &ast) {
  auto expr = ast.expr()->SemaAnalyze(*this);
  auto type = ast.type()->SemaAnalyze(*this);
  if (!expr || !type) return nullptr;
  // check if cast is valid
  if (type->IsReference() || !expr->CanCastTo(type)) {
    return LogError(ast.logger(), "invalid type casting");
  }
  return ast.set_ast_type(type->GetValueType(true));
}

TypePtr Analyzer::AnalyzeOn(UnaryAST &ast) {
  using UnaryOp = UnaryAST::UnaryOp;
  // get operand type
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
      if (opr->IsInteger() || opr->IsBool()) {
        ret = MakePrimType(Keyword::Bool, true);
      }
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
      if (!opr->IsRightValue()) ret = MakePointer(opr);
      break;
    }
    case UnaryOp::SizeOf: {
      ret = MakePrimType(Keyword::UInt32, true);
      break;
    }
    default: assert(false); return nullptr;
  }
  if (!ret) return LogError(ast.logger(), "invalid unary operation");
  if (ast.op() != UnaryOp::DeRef && !ret->IsRightValue()) {
    ret = ret->GetValueType(true);
  }
  return ast.set_ast_type(std::move(ret));
}

TypePtr Analyzer::AnalyzeOn(IndexAST &ast) {
  // get type of expression
  auto expr = ast.expr()->SemaAnalyze(*this);
  if (!expr || (!expr->IsPointer() &&
                !(expr->IsArray() && !expr->IsRightValue()))) {
    return LogError(ast.expr()->logger(),
                    "expression is not subscriptable");
  }
  if (expr->IsReference()) expr = expr->GetDerefedType();
  // get type of index
  auto index = ast.index()->SemaAnalyze(*this);
  if (!index || !index->IsInteger()) {
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
  TypePtr type;
  if (ast.expr()->IsId()) {
    // find function by id & update id
    auto id_ptr = static_cast<IdAST *>(ast.expr().get());
    auto setter = [id_ptr](const std::string &id) { id_ptr->set_id(id); };
    type = FindFuncType(ast.expr()->logger(), id_ptr->id(), args, setter);
    id_ptr->set_ast_type(type);
  }
  else {
    type = ast.expr()->SemaAnalyze(*this);
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
  assert(ret->IsReference() || ret->IsRightValue());
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
  // try to get type from symbol environment
  auto type = symbols_->GetItem(ast.id());
  if (!type) {
    // try to get mangled name from function mapping table
    auto name = funcs_->GetItem(ast.id());
    if (!name) {
      return LogError(ast.logger(), "identifier has not been defined",
                      ast.id());
    }
    else if (name) {
      ast.set_id(*name);
      type = symbols_->GetItem(*name);
      assert(type);
    }
  }
  return ast.set_ast_type(std::move(type));
}

TypePtr Analyzer::AnalyzeOn(StringAST &ast) {
  auto u8t = MakePrimType(Keyword::UInt8, true);
  auto cu8t = std::make_shared<ConstType>(std::move(u8t));
  auto strt = MakePointer(std::move(cu8t));
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
  if (type->IsReference()) {
    return LogError(ast.type()->logger(), "cannot initialize a reference");
  }
  assert(!type->IsRightValue());
  // check if is a valid initializer list
  if (ast.elems().size() > type->GetLength()) {
    return LogError(ast.logger(), "initializer list length exceeded");
  }
  for (std::size_t i = 0; i < ast.elems().size(); ++i) {
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
  auto ret = ast.ret() ? ast.ret()->SemaAnalyze(*this) : MakeVoid();
  if (!ret) return nullptr;
  if (!ret->IsReference() && !ret->IsRightValue()) {
    ret = ret->GetValueType(true);
  }
  // make function type
  auto type = std::make_shared<FuncType>(std::move(args), std::move(ret),
                                         false);
  return ast.set_ast_type(std::move(type));
}

TypePtr Analyzer::AnalyzeOn(VolaTypeAST &ast) {
  auto type = ast.type()->SemaAnalyze(*this);
  if (!type) return nullptr;
  if (type->IsReference()) {
    ast.type()->logger().LogWarning("volatile reference is meaningless");
    return ast.set_ast_type(std::move(type));
  }
  else {
    return ast.set_ast_type(std::make_shared<VolaType>(std::move(type)));
  }
}

TypePtr Analyzer::AnalyzeOn(ArrayTypeAST &ast) {
  // get base type
  auto base = ast.base()->SemaAnalyze(*this);
  if (!base) return nullptr;
  if (base->IsReference()) {
    return LogError(ast.base()->logger(), "base type cannot be reference");
  }
  // check type of length expression
  auto expr = ast.expr()->SemaAnalyze(*this);
  if (!expr) return nullptr;
  if (!expr->IsInteger()) {
    return LogError(ast.expr()->logger(),
                    "array length must be an integer");
  }
  // try to evaluate array length
  auto val = ast.expr()->Eval(eval_);
  auto len_ptr = std::get_if<std::uint64_t>(&*val);
  if (!val || !*len_ptr) {
    return LogError(ast.expr()->logger(), "invalid array length");
  }
  if (*len_ptr & (1ull << 63)) {
    ast.expr()->logger().LogWarning(
        "array length may be negative or a very large value");
  }
  // create array type
  auto arr = std::make_shared<ArrayType>(std::move(base), *len_ptr, false);
  return ast.set_ast_type(std::move(arr));
}

TypePtr Analyzer::AnalyzeOn(PointerTypeAST &ast) {
  // get base type
  auto base = ast.base()->SemaAnalyze(*this);
  if (!base) return nullptr;
  if (base->IsReference()) {
    return LogError(ast.base()->logger(), "base type cannot be reference");
  }
  if (!ast.is_var()) base = std::make_shared<ConstType>(std::move(base));
  // create pointer type
  auto type = MakePointer(std::move(base), false);
  return ast.set_ast_type(std::move(type));
}

TypePtr Analyzer::AnalyzeOn(RefTypeAST &ast) {
  // get base type
  auto base = ast.base()->SemaAnalyze(*this);
  if (!base) return nullptr;
  if (base->IsReference()) {
    return LogError(ast.logger(), "cannot reference a reference");
  }
  if (!ast.is_var()) base = std::make_shared<ConstType>(std::move(base));
  return ast.set_ast_type(std::make_shared<RefType>(std::move(base)));
}
