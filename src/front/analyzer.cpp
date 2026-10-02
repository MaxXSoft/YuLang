#include "front/analyzer.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <limits>
#include <sstream>
#include <unordered_set>
#include <utility>

namespace yulang::front {

using yulang::define::AccessAST;
using yulang::define::ArgElemAST;
using yulang::define::ArrayType;
using yulang::define::ArrayTypeAST;
using yulang::define::AsmAST;
using yulang::define::BaseAST;
using yulang::define::BinaryAST;
using yulang::define::BlockAST;
using yulang::define::BoolAST;
using yulang::define::CastAST;
using yulang::define::CharAST;
using yulang::define::ConstType;
using yulang::define::ControlAST;
using yulang::define::DeclareAST;
using yulang::define::EnumAST;
using yulang::define::EnumElemAST;
using yulang::define::EnumType;
using yulang::define::FloatAST;
using yulang::define::ForInAST;
using yulang::define::FunCallAST;
using yulang::define::FuncType;
using yulang::define::FuncTypeAST;
using yulang::define::FunDefAST;
using yulang::define::IdAST;
using yulang::define::IfAST;
using yulang::define::ImportAST;
using yulang::define::IndexAST;
using yulang::define::IntAST;
using yulang::define::Keyword;
using yulang::define::MakeEnv;
using yulang::define::MakeFuncMap;
using yulang::define::MakePointer;
using yulang::define::MakePrimType;
using yulang::define::MakeVoid;
using yulang::define::NullAST;
using yulang::define::Operator;
using yulang::define::PointerTypeAST;
using yulang::define::PrimTypeAST;
using yulang::define::Property;
using yulang::define::RefType;
using yulang::define::RefTypeAST;
using yulang::define::StringAST;
using yulang::define::StructAST;
using yulang::define::StructElemAST;
using yulang::define::StructType;
using yulang::define::TypeAliasAST;
using yulang::define::TypePairList;
using yulang::define::TypePtr;
using yulang::define::TypePtrList;
using yulang::define::UnaryAST;
using yulang::define::UserTypeAST;
using yulang::define::ValInitAST;
using yulang::define::VarLetDefAST;
using yulang::define::VarLetElemAST;
using yulang::define::VolaType;
using yulang::define::VolaTypeAST;
using yulang::define::WhenAST;
using yulang::define::WhenElemAST;
using yulang::define::WhileAST;

namespace {

// table of operator's name
const std::array kOperators = {YULANG_OPERATORS(YULANG_EXPAND_SECOND)};

// table of unary operator's name
const std::array kUnaOperators = {"+", "-", "!", "~", "*", "&"};

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
  } else {
    log.LogError(message, id);
  }
  return false;
}

// Strip only the value's qualifiers; pointer target qualifiers must survive.
TypePtr LiteralType(TypePtr type) {
  if (type->IsReference()) type = type->GetDerefedType();
  if (type->IsConst()) type = type->GetDeconstedType();
  return type->GetValueType(true);
}

bool FitsInteger(std::uint64_t value, const TypePtr &type,
                 bool negative = false) {
  if (!type->IsInteger()) return false;
  const auto bits = type->GetSize() * 8;
  if (negative) {
    return !type->IsUnsigned() && value <= (std::uint64_t{1} << (bits - 1));
  }
  const auto maximum =
      type->IsUnsigned()
          ? (std::numeric_limits<std::uint64_t>::max() >> (64 - bits))
          : ((std::uint64_t{1} << (bits - 1)) - 1);
  return value <= maximum;
}

bool IsIntegerOperation(Operator op) {
  switch (op) {
    case Operator::Add:
    case Operator::Sub:
    case Operator::Mul:
    case Operator::Div:
    case Operator::Mod:
    case Operator::And:
    case Operator::Or:
    case Operator::Xor:
    case Operator::Shl:
    case Operator::Shr:
      return true;
    default:
      return false;
  }
}

// Only these result positions can carry an unresolved literal type. In
// particular, identifiers, casts, calls, conditions and indices are barriers.
std::vector<BaseAST *> LiteralChildren(BaseAST &ast) {
  if (auto *unary = dynamic_cast<UnaryAST *>(&ast)) {
    using Op = UnaryAST::UnaryOp;
    if (!unary->op_func_id() &&
        (unary->op() == Op::Pos || unary->op() == Op::Neg ||
         unary->op() == Op::Not)) {
      return {unary->opr().get()};
    }
  } else if (auto *binary = dynamic_cast<BinaryAST *>(&ast)) {
    if (!binary->op_func_id() && IsIntegerOperation(binary->op()) &&
        !binary->ast_type()->IsPointer()) {
      return {binary->lhs().get(), binary->rhs().get()};
    }
  } else if (auto *block = dynamic_cast<BlockAST *>(&ast)) {
    if (!block->stmts().empty()) return {block->stmts().back().get()};
  } else if (auto *branch = dynamic_cast<IfAST *>(&ast)) {
    if (branch->else_then())
      return {branch->then().get(), branch->else_then().get()};
  } else if (auto *when = dynamic_cast<WhenAST *>(&ast)) {
    if (when->else_then()) {
      std::vector<BaseAST *> children;
      for (const auto &elem : when->elems()) children.push_back(elem.get());
      children.push_back(when->else_then().get());
      return children;
    }
  } else if (auto *arm = dynamic_cast<WhenElemAST *>(&ast)) {
    return {arm->body().get()};
  }
  return {};
}

IntAST *NegatedInteger(BaseAST &ast) {
  auto *unary = dynamic_cast<UnaryAST *>(&ast);
  if (!unary || unary->op() != UnaryAST::UnaryOp::Neg || unary->op_func_id()) {
    return nullptr;
  }
  return dynamic_cast<IntAST *>(unary->opr().get());
}

TypePtr IntegerUnaryContext(UnaryAST::UnaryOp op, const TypePtr &expected) {
  using Op = UnaryAST::UnaryOp;
  if (!expected || !expected->IsInteger()) return nullptr;
  return op == Op::Pos || op == Op::Neg || op == Op::Not ? expected : nullptr;
}

// Probe first, then commit: never partially rewrite a rejected candidate.
bool MatchLiteral(BaseAST &ast, const TypePtr &type, bool apply) {
  const auto value_type = LiteralType(type);
  auto *integer = dynamic_cast<IntAST *>(&ast);
  auto *negative = NegatedInteger(ast);
  if (integer || negative) {
    const auto magnitude = integer ? integer->value() : negative->value();
    if (!FitsInteger(magnitude, value_type, negative != nullptr)) return false;
    if (apply && negative) negative->set_ast_type(value_type);
  } else if (dynamic_cast<NullAST *>(&ast)) {
    if (!value_type->IsPointer()) return false;
  } else {
    if (!value_type->IsInteger() && !value_type->IsPointer()) {
      return ast.ast_type()->IsIdentical(type);
    }
    const auto children = LiteralChildren(ast);
    if (children.empty()) return ast.ast_type()->IsIdentical(type);
    if (!std::all_of(children.begin(), children.end(), [&](BaseAST *child) {
          return MatchLiteral(*child, value_type, apply);
        }))
      return false;
  }
  if (apply) ast.set_ast_type(value_type);
  return true;
}

bool AdaptLiteral(BaseAST &ast, const TypePtr &type) {
  return MatchLiteral(ast, type, false) && MatchLiteral(ast, type, true);
}

std::optional<TypePtr> AnalyzeNegativeLiteral(BaseAST &ast,
                                              const TypePtr &context) {
  if (!context || !NegatedInteger(ast)) return {};
  if (!AdaptLiteral(ast, context)) {
    return LogError(ast.logger(), "integer literal out of range");
  }
  return ast.ast_type();
}

void UnifyLiterals(BaseAST &lhs, BaseAST &rhs) {
  if (lhs.ast_type()->IsIdentical(rhs.ast_type())) return;
  if (!AdaptLiteral(rhs, lhs.ast_type())) AdaptLiteral(lhs, rhs.ast_type());
}

// Only result positions inherit a parameter's type. Locals, calls, casts,
// member access and other subexpressions still need their own type context.
bool NeedsInitializerContext(BaseAST &ast) {
  if (const auto *init = dynamic_cast<ValInitAST *>(&ast)) {
    return !init->type();
  }
  if (const auto *block = dynamic_cast<BlockAST *>(&ast)) {
    return !block->stmts().empty() &&
           NeedsInitializerContext(*block->stmts().back());
  }
  if (const auto *branch = dynamic_cast<IfAST *>(&ast)) {
    return NeedsInitializerContext(*branch->then()) ||
           (branch->else_then() &&
            NeedsInitializerContext(*branch->else_then()));
  }
  if (const auto *when = dynamic_cast<WhenAST *>(&ast)) {
    return std::any_of(when->elems().begin(), when->elems().end(),
                       [](const auto &elem) {
                         return NeedsInitializerContext(*elem);
                       }) ||
           (when->else_then() && NeedsInitializerContext(*when->else_then()));
  }
  if (const auto *arm = dynamic_cast<WhenElemAST *>(&ast)) {
    return NeedsInitializerContext(*arm->body());
  }
  return false;
}

bool MatchArguments(const TypePtr &function, TypePtrList &args,
                    const std::vector<BaseAST *> &exprs, bool apply) {
  const auto params = function->GetArgsType();
  if (!params || params->size() != args.size()) return false;
  auto projected = args;
  for (std::size_t i = 0; i < args.size(); ++i) {
    // An omitted initializer is checked once, after its context is unique.
    if (!args[i]) {
      assert(!apply);
      if (!(*params)[i]->IsStruct() && !(*params)[i]->IsArray()) return false;
      projected[i] = LiteralType((*params)[i]);
      continue;
    }
    if (!MatchLiteral(*exprs[i], (*params)[i], false)) return false;
    if (!(*params)[i]->IsIdentical(args[i])) {
      projected[i] = LiteralType((*params)[i]);
    }
  }
  if (!function->GetReturnType(projected)) return false;
  if (apply) {
    for (std::size_t i = 0; i < args.size(); ++i) {
      if (!(*params)[i]->IsIdentical(args[i])) {
        AdaptLiteral(*exprs[i], (*params)[i]);
      }
    }
    args = std::move(projected);
  }
  return true;
}

}  // namespace

TypePtr Analyzer::AnalyzeExpr(BaseAST &ast, TypePtr expected) {
  const auto previous = std::exchange(expected_, std::move(expected));
  const xstl::Guard context([this, previous] { expected_ = previous; });
  return ast.SemaAnalyze(*this);
}

TypePtr Analyzer::FindLocalFunction(const std::string &id) const {
  for (auto env = symbols_; env && !env->is_root(); env = env->outer()) {
    if (const auto local = env->GetItem(id, false)) {
      return local->IsFunction() ? local : nullptr;
    }
  }
  return nullptr;
}

TypePtr Analyzer::ResolveCall(const Logger &log, const std::string &id,
                              TypePtrList &args,
                              const std::vector<BaseAST *> &exprs,
                              const IdSetter &id_setter) {
  // A local callable binding takes precedence over named overloads. Keep the
  // separate overload namespace for non-callable receiver values such as x.x().
  if (auto local = FindLocalFunction(id)) {
    if (!MatchArguments(local, args, exprs, true)) {
      return LogError(log, "invalid function call", id);
    }
    id_setter(id);
    return local;
  }
  // Preserve the established meaning of calls such as f(1) with f(i32).
  const auto mangled = MangleFuncName(id, args);
  auto exact = symbols_->GetItem(mangled);
  if (exact && MatchArguments(exact, args, exprs, true)) {
    id_setter(mangled);
    return exact;
  }
  // A function value or an extern declaration already determines the signature.
  if (auto direct = symbols_->GetItem(id); direct && direct->IsFunction()) {
    if (!MatchArguments(direct, args, exprs, true)) {
      return LogError(log, "invalid function call", id);
    }
    id_setter(id);
    return direct;
  }
  TypePtr selected;
  std::string selected_id;
  std::unordered_set<std::string> seen;
  for (auto env = funcs_; env; env = env->outer()) {
    const auto names = env->GetItem(id, false);
    if (!names) continue;
    for (const auto &name : *names) {
      if (!seen.insert(name).second) continue;
      auto candidate = symbols_->GetItem(name);
      if (!candidate || !MatchArguments(candidate, args, exprs, false))
        continue;
      if (selected) return LogError(log, "ambiguous function call", id);
      selected = std::move(candidate);
      selected_id = name;
    }
  }
  if (!selected) return LogError(log, "no matching function", id);
  MatchArguments(selected, args, exprs, true);
  id_setter(selected_id);
  return selected;
}

TypePtrList Analyzer::GetCallCandidates(const std::string &id) const {
  if (const auto direct = symbols_->GetItem(id);
      direct && direct->IsFunction()) {
    return {direct};
  }
  TypePtrList candidates;
  std::unordered_set<std::string> seen;
  for (auto env = funcs_; env; env = env->outer()) {
    const auto names = env->GetItem(id, false);
    if (!names) continue;
    for (const auto &name : *names) {
      if (seen.insert(name).second) {
        if (const auto type = symbols_->GetItem(name))
          candidates.push_back(type);
      }
    }
  }
  return candidates;
}

bool Analyzer::AnalyzeInferredArguments(TypePtrList &args,
                                        const std::vector<BaseAST *> &exprs,
                                        const TypePtrList &candidates) {
  std::vector<TypePtrList> viable;
  for (const auto &candidate : candidates) {
    if (const auto params = candidate->GetArgsType();
        params && MatchArguments(candidate, args, exprs, false)) {
      viable.push_back(*params);
    }
  }
  for (std::size_t i = 0; i < args.size(); ++i) {
    if (args[i]) continue;
    if (viable.empty()) {
      return LogErrorBool(exprs[i]->logger(), "no matching function", "");
    }
    const auto &context = viable.front()[i];
    for (const auto &candidate : viable) {
      if (context->GetTypeId() != candidate[i]->GetTypeId()) {
        return LogErrorBool(exprs[i]->logger(),
                            "ambiguous initializer type; specify [Type]", "");
      }
    }
    args[i] = AnalyzeExpr(*exprs[i], context);
    if (!args[i]) return false;
  }
  return true;
}

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
                           const TypePtr &type) {
  if (user_types_->GetItem(id, false)) {
    LogError(log, "type has already been defined", id);
    return false;
  }
  user_types_->AddItem(id, type);
  return true;
}

TypePtr Analyzer::FindFuncType(const Logger &log, const std::string &id,
                               const TypePtrList &args,
                               const IdSetter &id_setter) {
  const auto mangled = MangleFuncName(id, args);
  auto type = symbols_->GetItem(mangled);
  if (!type) {
    type = symbols_->GetItem(id);
    if (!type || !type->IsFunction()) {
      return LogError(log, "function not found", id);
    }
    if (id_setter) id_setter(id);
  } else {
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
      return LogErrorBool(log,
                          "reference cannot be initialized "
                          "with a right value",
                          id);
    }
    if (init->IsConst() && !type->GetDerefedType()->IsConst()) {
      return LogErrorBool(log,
                          "variable reference cannot be initialized "
                          "with a constant",
                          id);
    }
  }
  return true;
}

std::optional<TypePtr> Analyzer::CheckOpOverload(
    const Logger &log, const std::string &op_name, TypePtrList &args,
    const IdSetter &id_setter, const std::vector<BaseAST *> &exprs) {
  bool found = false;
  for (auto env = funcs_; env && !found; env = env->outer()) {
    const auto names = env->GetItem(op_name, false);
    if (!names) continue;
    for (const auto &name : *names) {
      const auto candidate = symbols_->GetItem(name);
      if (candidate && MatchArguments(candidate, args, exprs, false)) {
        found = true;
        break;
      }
    }
  }
  if (!found) return {};
  auto type = ResolveCall(log, op_name, args, exprs, id_setter);
  if (!type) return nullptr;
  return type->GetReturnType(args);
}

TypePtr Analyzer::AnalyzeOn(VarLetDefAST &ast) {
  const auto previous = std::exchange(last_prop_, ast.prop());
  const xstl::Guard property([this, previous] { last_prop_ = previous; });
  bool valid = true;
  for (const auto &i : ast.defs()) {
    const auto errors = Logger::error_num();
    if (!AnalyzeExpr(*i) || Logger::error_num() != errors) {
      valid = false;
      continue;
    }
    // run evaluation on var/let definitions for constant definitions
    i->Eval(eval_);
    valid &= Logger::error_num() == errors;
  }
  if (!valid) return nullptr;
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(FunDefAST &ast) {
  // get types of arguments
  const auto args_env = NewEnv();
  const auto eval_env = eval_.NewEnv();
  TypePtrList args;
  for (const auto &i : ast.args()) {
    auto type = AnalyzeExpr(*i);
    if (!type) return nullptr;
    args.push_back(std::move(type));
  }
  // get return type
  auto ret = ast.type() ? AnalyzeExpr(*ast.type()) : MakeVoid();
  if (!ret) return nullptr;
  const auto previous = std::exchange(cur_ret_, ret);
  const xstl::Guard return_type([this, previous] { cur_ret_ = previous; });
  if (!ret->IsReference() && !ret->IsRightValue()) {
    ret = ret->GetValueType(true);
  }
  // perform function name mangling
  auto id = ast.id();
  const auto org_id = id;
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
  auto type = std::make_shared<FuncType>(std::move(args), std::move(ret), true);
  symbols_->outer()->AddItem(id, type);
  // add function type info to function mapping table
  auto &overloads = funcs_->outer()->AccessItem(org_id);
  if (!overloads) overloads.emplace();
  overloads->push_back(id);
  // analyze function's body
  if (ast.body()) {
    const auto body_ret = AnalyzeExpr(*ast.body(), cur_ret_);
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
  auto type = AnalyzeExpr(*ast.type());
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
  const auto org_id = id;
  if (type->IsFunction() && ast.prop() != Property::Extern) {
    const auto args = type->GetArgsType();
    if (!args) {
      return LogError(ast.logger(), "expected function argument types");
    }
    id = MangleFuncName(id, *args);
    ast.set_id(id);
  }
  // add type info to environment
  if (!symbols_->is_root() && !type->IsFunction()) {
    return LogError(ast.logger(),
                    "cannot declare non-function "
                    "in function",
                    id);
  }
  if (symbols_->GetItem(id, false)) {
    return LogError(ast.logger(), "symbol has already been defined", id);
  }
  symbols_->AddItem(id, type);
  if (type->IsFunction()) {
    auto &overloads = funcs_->AccessItem(org_id);
    if (!overloads) overloads.emplace();
    overloads->push_back(id);
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(TypeAliasAST &ast) {
  const auto type = AnalyzeExpr(*ast.type());
  if (!type) return nullptr;
  // add type alias to environment
  if (!AddUserType(ast.logger(), ast.id(), type)) {
    return nullptr;
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(StructAST &ast) {
  TypePairList elems;
  // create an empty struct type
  auto type = std::make_shared<StructType>(elems, ast.id(), false);
  if (!AddUserType(ast.logger(), ast.id(), type)) return nullptr;
  xstl::Guard registration(
      [this, &ast] { user_types_->RemoveItem(ast.id(), false); });
  // analyze elements
  std::unordered_set<std::string_view> names;
  for (const auto &i : ast.defs()) {
    auto elem = AnalyzeExpr(*i);
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
    elems.emplace_back(std::string(last_struct_elem_name_), std::move(elem));
  }
  // update struct type
  // TODO(YuLang): circular reference!
  type->set_elems(std::move(elems));
  ast.set_ast_type(std::move(type));
  registration.Invalidate();
  return MakeVoid();
}

TypePtr Analyzer::AnalyzeOn(EnumAST &ast) {
  // get type
  auto type = ast.type() ? AnalyzeExpr(*ast.type())
                         : MakePrimType(Keyword::Int32, false);
  if (!type) return nullptr;
  if (!type->IsInteger() || type->IsReference()) {
    return LogError(ast.logger(), "enumuration's type must be an integer type");
  }
  last_enum_type_ = type;
  // get all elements
  EnumType::ElemSet elems;
  for (const auto &i : ast.defs()) {
    if (!AnalyzeExpr(*i)) return nullptr;
    auto [_, succ] = elems.insert(last_enum_elem_name_);
    if (!succ) {
      return LogError(i->logger(), "conflicted enumeration element name",
                      last_enum_elem_name_);
    }
  }
  // Validate both namespaces before publishing the enumeration type.
  if (symbols_->GetItem(ast.id(), false)) {
    return LogError(ast.logger(),
                    "enumeration name has already been used "
                    "by another symbol",
                    ast.id());
  }
  const auto enum_type = std::make_shared<EnumType>(
      std::move(type), std::move(elems), ast.id(), false);
  if (!AddUserType(ast.logger(), ast.id(), enum_type)) return nullptr;
  // add enumeration right value to symbol environment
  symbols_->AddItem(ast.id(), enum_type->GetValueType(true));
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(ImportAST &ast) {
  bool valid = true;
  for (const auto &i : ast.defs()) {
    const auto errors = Logger::error_num();
    if (!AnalyzeExpr(*i) || Logger::error_num() != errors) {
      valid = false;
      continue;
    }
    // run evaluation on imported ASTs for constant definitions
    i->Eval(eval_);
    valid &= Logger::error_num() == errors;
  }
  if (!valid) return nullptr;
  return ast.set_ast_type(MakeVoid());
}

// Keep the exhaustive type/operator dispatch together for semantic review.
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TypePtr Analyzer::AnalyzeOn(VarLetElemAST &ast) {
  const auto &log = ast.logger();
  const auto &id = ast.id();
  // try to get variable type and initializer type
  TypePtr type;
  TypePtr init;
  if (ast.type()) {
    type = AnalyzeExpr(*ast.type());
    if (!type) return nullptr;
  }
  if (ast.init()) {
    init = AnalyzeExpr(*ast.init(), type);
    if (!init) return nullptr;
  }
  // check if is ill-formed
  if (last_prop_ == Property::Public || last_prop_ == Property::Extern) {
    if (!type) {
      return LogError(log,
                      "'public' definitions must "
                      "specify type explicitly");
    }
  } else if ((ast.is_var() && !type && !init) || (!ast.is_var() && !init)) {
    return LogError(log, "initializer required");
  }
  // Local definitions introduce a new binding after their initializer is
  // analyzed. Globals still reject same-scope duplicates.
  if (!cur_ret_ && symbols_->GetItem(id, false)) {
    return LogError(log, "identifier has already been defined", id);
  }
  // check types
  TypePtr sym_type;
  if (type) {
    assert(!type->IsRightValue());
    if (init && !CheckInit(log, type, init, id)) return nullptr;
    // check for reference types
    if (type->IsReference()) {
      if (!init) {
        return LogError(log,
                        "cannot define a reference "
                        "without initialization",
                        id);
      }
      if (ast.is_var()) {
        log.LogWarning("references should only be defined with 'let'");
      }
    }
    sym_type = std::move(type);
  } else {
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
  if (!sym_type->IsReference()) {
    if (ast.is_var()) {
      if (sym_type->IsConst()) sym_type = sym_type->GetDeconstedType();
    } else if (!sym_type->IsConst()) {
      sym_type = std::make_shared<ConstType>(std::move(sym_type));
    }
  }
  symbols_->AccessItem(id) = sym_type;
  ast.set_ast_type(sym_type);
  return MakeVoid();
}

TypePtr Analyzer::AnalyzeOn(ArgElemAST &ast) {
  // get type
  auto type = AnalyzeExpr(*ast.type());
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
  eval_.Mask(ast.id());
  return ast.set_ast_type(type);
}

TypePtr Analyzer::AnalyzeOn(StructElemAST &ast) {
  // get type
  const auto type = AnalyzeExpr(*ast.type());
  if (!type) return nullptr;
  // set last argument info (for structures)
  assert(!type->IsRightValue());
  if (type->IsReference()) {
    return LogError(ast.logger(),
                    "type of structure element cannot be reference");
  }
  last_struct_elem_name_ = ast.id();
  return ast.set_ast_type(type);
}

TypePtr Analyzer::AnalyzeOn(EnumElemAST &ast) {
  // check type of initialization expression
  if (ast.expr()) {
    const auto type = AnalyzeExpr(*ast.expr(), last_enum_type_);
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
  const auto env = NewEnv();
  const auto eval_env = eval_.NewEnv();
  auto ret = MakeVoid();
  bool valid = true;
  for (std::size_t i = 0; i < ast.stmts().size(); ++i) {
    const auto errors = Logger::error_num();
    auto type = AnalyzeExpr(*ast.stmts()[i],
                            i + 1 == ast.stmts().size() ? expected_ : nullptr);
    if (!type || Logger::error_num() != errors) {
      valid = false;
      continue;
    }
    if (i == ast.stmts().size() - 1) ret = std::move(type);
  }
  if (!valid) return nullptr;
  return ast.set_ast_type(ret);
}

TypePtr Analyzer::AnalyzeOn(IfAST &ast) {
  // check condition
  const auto cond = AnalyzeExpr(*ast.cond());
  if (!cond) return nullptr;
  if (!cond->IsBool()) {
    return LogError(ast.cond()->logger(), "condition must be a boolean");
  }
  // check then block
  auto then = AnalyzeExpr(*ast.then(), expected_);
  if (!then) return nullptr;
  // check else then block
  auto else_then =
      ast.else_then() ? AnalyzeExpr(*ast.else_then(), expected_) : MakeVoid();
  if (!else_then) return nullptr;
  if (ast.else_then()) {
    UnifyLiterals(*ast.then(), *ast.else_then());
    then = ast.then()->ast_type();
    else_then = ast.else_then()->ast_type();
  }
  // create return type
  auto ret = MakeVoid();
  if (then->IsIdentical(else_then)) {
    // if one of return value is right value, return right value
    ret = then->IsRightValue() ? std::move(then) : std::move(else_then);
  }
  return ast.set_ast_type(ret);
}

TypePtr Analyzer::AnalyzeOn(WhenAST &ast) {
  // get type of expression
  last_when_expr_type_ = AnalyzeExpr(*ast.expr());
  if (!last_when_expr_type_) return nullptr;
  const auto condition_type = last_when_expr_type_;
  std::vector<BaseAST *> branches;
  for (const auto &i : ast.elems()) {
    last_when_expr_type_ = condition_type;
    if (!AnalyzeExpr(*i, expected_)) return nullptr;
    branches.push_back(i.get());
  }
  if (!ast.else_then()) return ast.set_ast_type(MakeVoid());
  if (!AnalyzeExpr(*ast.else_then(), expected_)) return nullptr;
  branches.push_back(ast.else_then().get());
  for (auto *branch : branches) UnifyLiterals(*branches.front(), *branch);
  auto ret = branches.front()->ast_type();
  for (auto *branch : branches) {
    if (!branch->ast_type()->IsIdentical(ret) && !AdaptLiteral(*branch, ret))
      return ast.set_ast_type(MakeVoid());
    if (branch->ast_type()->IsRightValue()) ret = branch->ast_type();
  }
  return ast.set_ast_type(ret);
}

TypePtr Analyzer::AnalyzeOn(WhileAST &ast) {
  // check condition
  const auto cond = AnalyzeExpr(*ast.cond());
  if (!cond) return nullptr;
  if (!cond->IsBool()) {
    return LogError(ast.cond()->logger(), "condition must be a boolean");
  }
  // check body
  ++in_loop_;
  const xstl::Guard loop([this] { --in_loop_; });
  if (!AnalyzeExpr(*ast.body())) return nullptr;
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(ForInAST &ast) {
  const auto env = NewEnv();
  const auto eval_env = eval_.NewEnv();
  // get type of expression
  auto expr = AnalyzeExpr(*ast.expr());
  if (!expr) return nullptr;
  if (expr->IsRightValue()) expr = expr->GetValueType(false);
  // find iterator
  const TypePtrList args = {std::move(expr)};
  const auto next =
      FindFuncType(ast.expr()->logger(), "next", args,
                   [&ast](const std::string &id) { ast.set_next_id(id); });
  const auto last =
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
  eval_.Mask(ast.id());
  ast.set_id_type(type);
  // check body
  ++in_loop_;
  const xstl::Guard loop([this] { --in_loop_; });
  if (!AnalyzeExpr(*ast.body())) return nullptr;
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(AsmAST &ast) {
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(ControlAST &ast) {
  switch (ast.type()) {
    case Keyword::Break:
    case Keyword::Continue: {
      // check if is in a loop
      if (!in_loop_) {
        return LogError(ast.logger(), "using break/continue outside the loop");
      }
      break;
    }
    case Keyword::Return: {
      if (!cur_ret_) {
        return LogError(ast.logger(), "using return outside a function");
      }
      const auto type =
          ast.expr() ? AnalyzeExpr(*ast.expr(), cur_ret_) : MakeVoid();
      // check if is compatible
      assert(cur_ret_->IsVoid() || !cur_ret_->IsRightValue());
      if (!type || !CheckInit(ast.logger(), cur_ret_, type)) return nullptr;
      break;
    }
    default:
      assert(false);
  }
  return ast.set_ast_type(MakeVoid());
}

TypePtr Analyzer::AnalyzeOn(WhenElemAST &ast) {
  // check conditions
  const auto condition_type = last_when_expr_type_;
  for (const auto &i : ast.conds()) {
    const auto cond = AnalyzeExpr(*i, condition_type);
    if (!cond) return nullptr;
    if (!cond->IsIdentical(condition_type)) {
      return LogError(i->logger(), "condition type mismatch");
    }
  }
  // check body
  const auto body = AnalyzeExpr(*ast.body(), expected_);
  if (!body) return nullptr;
  return ast.set_ast_type(body);
}

// Keep the exhaustive type/operator dispatch together for semantic review.
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TypePtr Analyzer::AnalyzeOn(BinaryAST &ast) {
  // get lhs and rhs type
  auto context = IsIntegerOperation(ast.op()) ? expected_ : nullptr;
  if (context && !context->IsInteger()) context.reset();
  auto lhs = AnalyzeExpr(*ast.lhs(), context);
  if (define::IsOperatorAssign(ast.op()) && lhs) {
    context = lhs->IsReference() ? lhs->GetDerefedType() : lhs;
    if (lhs->IsPointer() && ast.op() != Operator::Assign) context.reset();
  }
  auto rhs = AnalyzeExpr(*ast.rhs(), context);
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
  // Reconcile unresolved literal trees without reanalyzing scopes or calls.
  if (!define::IsOperatorAssign(ast.op()) &&
      !(IsIntegerOperation(ast.op()) &&
        (lhs->IsPointer() || rhs->IsPointer()))) {
    UnifyLiterals(*ast.lhs(), *ast.rhs());
    lhs = ast.lhs()->ast_type();
    rhs = ast.rhs()->ast_type();
  }
  // check if operator was overloaded
  if (!lhs->IsBasic() || !rhs->IsBasic()) {
    TypePtrList args = {lhs, rhs};
    const auto *op_name = kOperators[static_cast<int>(ast.op())];
    auto ret = CheckOpOverload(
        ast.logger(), op_name, args,
        [&ast](const std::string &id) { ast.set_op_func_id(id); },
        {ast.lhs().get(), ast.rhs().get()});
    if (ret) return ast.set_ast_type(*ret);
  }
  // normal binary operation
  TypePtr ret;
  switch (ast.op()) {
    case Operator::Add:
    case Operator::Sub: {
      if (lhs->IsPointer() || rhs->IsPointer()) {
        // pointer operation
        if (lhs->IsPointer() && rhs->IsInteger()) {
          ret = lhs;
        } else if (rhs->IsPointer() && lhs->IsInteger() &&
                   ast.op() != Operator::Sub) {
          ret = rhs;
        } else {
          return LogError(ast.logger(), "invalid pointer operation");
        }
        break;
      }
      // fall through
    }
    case Operator::Mul:
    case Operator::Div:
    case Operator::Mod: {
      // float binary operation
      if (lhs->IsFloat() && lhs->IsIdentical(rhs)) {
        ret = lhs;
        break;
      }
      // fall through
    }
    case Operator::And:
    case Operator::Or:
    case Operator::Xor:
    case Operator::Shl:
    case Operator::Shr: {
      // int binary operation
      if (lhs->IsInteger() && lhs->IsIdentical(rhs)) ret = lhs;
      break;
    }
    case Operator::Less:
    case Operator::LessEqual:
    case Operator::Great:
    case Operator::GreatEqual: {
      // int/float binary operation
      if ((lhs->IsInteger() || lhs->IsFloat() || lhs->IsPointer()) &&
          lhs->IsIdentical(rhs)) {
        ret = MakePrimType(Keyword::Bool, true);
      }
      break;
    }
    case Operator::LogicAnd:
    case Operator::LogicOr: {
      // bool binary operation
      if (lhs->IsBool() && lhs->IsIdentical(rhs)) ret = lhs;
      break;
    }
    case Operator::Equal:
    case Operator::NotEqual: {
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
    case Operator::AssAdd:
    case Operator::AssSub: {
      // pointer operation
      if (lhs->IsPointer() && !lhs->IsRightValue() && !lhs->IsConst() &&
          rhs->IsInteger()) {
        ret = MakeVoid();
        break;
      }
      // fall through
    }
    case Operator::AssMul:
    case Operator::AssDiv:
    case Operator::AssMod: {
      // float binary operation
      if (lhs->IsFloat() && lhs->CanAccept(rhs)) {
        ret = MakeVoid();
        break;
      }
      // fall through
    }
    case Operator::AssAnd:
    case Operator::AssOr:
    case Operator::AssXor:
    case Operator::AssShl:
    case Operator::AssShr: {
      // int binary operation
      if (lhs->IsInteger() && lhs->CanAccept(rhs)) ret = MakeVoid();
      break;
    }
    default:
      assert(false);
      return nullptr;
  }
  if (!ret) return LogError(ast.logger(), "invalid binary operation");
  if (!ret->IsRightValue()) ret = ret->GetValueType(true);
  return ast.set_ast_type(ret);
}

TypePtr Analyzer::AnalyzeOn(AccessAST &ast) {
  // get expression type & id
  auto expr = AnalyzeExpr(*ast.expr());
  // handle access
  TypePtr ret;
  if (expr) {
    if (expr->IsEnum() && expr->GetElem(ast.id())) {
      // check if is enumeration access
      assert(expr->IsRightValue());
      ret = std::move(expr);
    } else if (expr->IsStruct() && !expr->IsRightValue()) {
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
  return ast.set_ast_type(ret);
}

TypePtr Analyzer::AnalyzeOn(CastAST &ast) {
  const auto expr = AnalyzeExpr(*ast.expr());
  const auto type = AnalyzeExpr(*ast.type());
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
  const auto context = IntegerUnaryContext(ast.op(), expected_);
  if (auto literal = AnalyzeNegativeLiteral(ast, context)) return *literal;
  auto opr = AnalyzeExpr(*ast.opr(), context);
  if (!opr || opr->IsVoid()) {
    return LogError(ast.opr()->logger(), "invalid operand");
  }
  if (opr->IsReference()) opr = opr->GetDerefedType();
  // check if operator was overloaded
  // NOTE: 'sizeof' cannot be overloaded
  if (!opr->IsBasic() && ast.op() != UnaryOp::SizeOf) {
    TypePtrList args = {opr};
    const auto *op_name = kUnaOperators[static_cast<int>(ast.op())];
    auto ret = CheckOpOverload(
        ast.logger(), op_name, args,
        [&ast](const std::string &id) { ast.set_op_func_id(id); },
        {ast.opr().get()});
    if (ret) return ast.set_ast_type(*ret);
  }
  // normal unary operations
  TypePtr ret;
  switch (ast.op()) {
    case UnaryOp::Pos:
    case UnaryOp::Neg: {
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
      ret = MakePrimType(Keyword::USize, true);
      break;
    }
    default:
      assert(false);
      return nullptr;
  }
  if (!ret) return LogError(ast.logger(), "invalid unary operation");
  if (ast.op() != UnaryOp::DeRef && !ret->IsRightValue()) {
    ret = ret->GetValueType(true);
  }
  return ast.set_ast_type(ret);
}

TypePtr Analyzer::AnalyzeOn(IndexAST &ast) {
  // get type of expression
  auto expr = AnalyzeExpr(*ast.expr());
  if (!expr ||
      (!expr->IsPointer() && (!expr->IsArray() || expr->IsRightValue()))) {
    return LogError(ast.expr()->logger(), "expression is not subscriptable");
  }
  if (expr->IsReference()) expr = expr->GetDerefedType();
  // get type of index
  const auto index = AnalyzeExpr(*ast.index());
  if (!index || !index->IsInteger()) {
    return LogError(ast.index()->logger(), "invalid index");
  }
  // get return type
  const auto ret = expr->GetDerefedType();
  if (expr->IsArray()) {
    auto val = ast.index()->Eval(eval_);
    if (val) {
      // check if out of bounds
      const auto *val_ptr = std::get_if<std::uint64_t>(&*val);
      assert(val_ptr);
      if (*val_ptr >= expr->GetLength()) {
        ast.index()->logger().LogWarning("subscript out of bounds");
      }
    }
  }
  return ast.set_ast_type(ret);
}

TypePtr Analyzer::AnalyzeOn(FunCallAST &ast) {
  // Analyze known arguments first; omitted aggregate types need a signature.
  TypePtrList args;
  std::vector<BaseAST *> exprs;
  bool needs_context = false;
  for (const auto &i : ast.args()) {
    exprs.push_back(i.get());
    if (NeedsInitializerContext(*i)) {
      needs_context = true;
      args.push_back(nullptr);
    } else {
      auto arg = AnalyzeExpr(*i);
      if (!arg) return nullptr;
      args.push_back(std::move(arg));
    }
  }
  // get function type
  TypePtr type;
  if (ast.expr()->IsId()) {
    // find function by id & update id
    // The preceding AST tag proves this is an IdAST.
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
    auto *id_ptr = static_cast<IdAST *>(ast.expr().get());
    const auto setter = [id_ptr](const std::string &id) { id_ptr->set_id(id); };
    if (needs_context && !AnalyzeInferredArguments(
                             args, exprs, GetCallCandidates(id_ptr->id()))) {
      return nullptr;
    }
    type = ResolveCall(ast.expr()->logger(), id_ptr->id(), args, exprs, setter);
    id_ptr->set_ast_type(type);
  } else {
    type = AnalyzeExpr(*ast.expr());
  }
  // check function type
  if (!type || !type->IsFunction()) {
    return LogError(ast.expr()->logger(), "calling a non-function");
  }
  // A function-valued expression also provides parameter contexts.
  if (!ast.expr()->IsId()) {
    if (needs_context && !AnalyzeInferredArguments(args, exprs, {type})) {
      return nullptr;
    }
    if (!MatchArguments(type, args, exprs, true)) {
      return LogError(ast.logger(), "invalid function call");
    }
  }
  // get return type
  const auto ret = type->GetReturnType(args);
  if (!ret) {
    return LogError(ast.expr()->logger(), "invalid function call");
  }
  assert(ret->IsReference() || ret->IsRightValue());
  return ast.set_ast_type(ret);
}

TypePtr Analyzer::AnalyzeOn(IntAST &ast) {
  if (expected_ && expected_->IsInteger()) {
    if (!FitsInteger(ast.value(), expected_)) {
      return LogError(ast.logger(), "integer literal out of range");
    }
    return ast.set_ast_type(LiteralType(expected_));
  }
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
    if (!name || name->size() != 1) {
      return LogError(ast.logger(), "identifier has not been defined",
                      ast.id());
    }
    if (name) {
      ast.set_id(name->front());
      type = symbols_->GetItem(name->front());
      assert(type);
    }
  }
  return ast.set_ast_type(type);
}

TypePtr Analyzer::AnalyzeOn(StringAST &ast) {
  auto u8t = MakePrimType(Keyword::UInt8, true);
  auto cu8t = std::make_shared<ConstType>(std::move(u8t));
  const auto strt = MakePointer(std::move(cu8t));
  return ast.set_ast_type(strt);
}

TypePtr Analyzer::AnalyzeOn(BoolAST &ast) {
  return ast.set_ast_type(MakePrimType(Keyword::Bool, true));
}

TypePtr Analyzer::AnalyzeOn(NullAST &ast) {
  if (expected_ && expected_->IsPointer()) {
    return ast.set_ast_type(LiteralType(expected_));
  }
  return ast.set_ast_type(MakePrimType(Keyword::Null, true));
}

TypePtr Analyzer::AnalyzeOn(ValInitAST &ast) {
  auto type = ast.type() ? AnalyzeExpr(*ast.type()) : expected_;
  if (!ast.type() && !type) {
    return LogError(ast.logger(),
                    "cannot infer initializer type; specify [Type]");
  }
  if (!type) return nullptr;
  if (type->IsReference()) {
    return LogError(ast.logger(), "cannot initialize a reference");
  }
  if (!type->IsStruct() && !type->IsArray()) {
    return LogError(ast.logger(),
                    "initializer requires a structure or array type");
  }
  if (!ast.type()) type = LiteralType(type)->GetValueType(false);
  assert(!type->IsRightValue());
  // check if is a valid initializer list
  if (ast.elems().size() > type->GetLength()) {
    return LogError(ast.logger(), "initializer list length exceeded");
  }
  for (std::size_t i = 0; i < ast.elems().size(); ++i) {
    // get type of element
    const auto elem = AnalyzeExpr(*ast.elems()[i], type->GetElem(i));
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
  const auto type = user_types_->GetItem(ast.id());
  if (!type) {
    return LogError(ast.logger(), "type has not been defined", ast.id());
  }
  return ast.set_ast_type(type);
}

TypePtr Analyzer::AnalyzeOn(FuncTypeAST &ast) {
  // get type of arguments
  TypePtrList args;
  for (const auto &i : ast.args()) {
    auto arg = AnalyzeExpr(*i);
    if (!arg) return nullptr;
    args.push_back(std::move(arg));
  }
  // get return type
  auto ret = ast.ret() ? AnalyzeExpr(*ast.ret()) : MakeVoid();
  if (!ret) return nullptr;
  if (!ret->IsReference() && !ret->IsRightValue()) {
    ret = ret->GetValueType(true);
  }
  // make function type
  auto type =
      std::make_shared<FuncType>(std::move(args), std::move(ret), false);
  return ast.set_ast_type(std::move(type));
}

TypePtr Analyzer::AnalyzeOn(VolaTypeAST &ast) {
  auto type = AnalyzeExpr(*ast.type());
  if (!type) return nullptr;
  if (type->IsReference()) {
    ast.type()->logger().LogWarning("volatile reference is meaningless");
    return ast.set_ast_type(type);
  }
  return ast.set_ast_type(std::make_shared<VolaType>(std::move(type)));
}

TypePtr Analyzer::AnalyzeOn(ArrayTypeAST &ast) {
  // get base type
  auto base = AnalyzeExpr(*ast.base());
  if (!base) return nullptr;
  if (base->IsReference()) {
    return LogError(ast.base()->logger(), "base type cannot be reference");
  }
  // check type of length expression
  const auto expr = AnalyzeExpr(*ast.expr());
  if (!expr) return nullptr;
  if (!expr->IsInteger()) {
    return LogError(ast.expr()->logger(), "array length must be an integer");
  }
  // try to evaluate array length
  auto val = ast.expr()->Eval(eval_);
  if (!val) {
    return LogError(ast.expr()->logger(), "invalid array length");
  }
  const auto *len_ptr = std::get_if<std::uint64_t>(&*val);
  if (!len_ptr || !*len_ptr) {
    return LogError(ast.expr()->logger(), "invalid array length");
  }
  if (*len_ptr & (std::uint64_t{1} << 63U)) {
    ast.expr()->logger().LogWarning(
        "array length may be negative or a very large value");
  }
  // create array type
  auto arr = std::make_shared<ArrayType>(std::move(base), *len_ptr, false);
  return ast.set_ast_type(std::move(arr));
}

TypePtr Analyzer::AnalyzeOn(PointerTypeAST &ast) {
  // get base type
  auto base = AnalyzeExpr(*ast.base());
  if (!base) return nullptr;
  if (base->IsReference()) {
    return LogError(ast.base()->logger(), "base type cannot be reference");
  }
  if (!ast.is_var()) base = std::make_shared<ConstType>(std::move(base));
  // create pointer type
  const auto type = MakePointer(base, false);
  return ast.set_ast_type(type);
}

TypePtr Analyzer::AnalyzeOn(RefTypeAST &ast) {
  // get base type
  auto base = AnalyzeExpr(*ast.base());
  if (!base) return nullptr;
  if (base->IsReference()) {
    return LogError(ast.logger(), "cannot reference a reference");
  }
  if (!ast.is_var()) base = std::make_shared<ConstType>(std::move(base));
  return ast.set_ast_type(std::make_shared<RefType>(std::move(base)));
}

}  // namespace yulang::front
