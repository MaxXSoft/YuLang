#include <algorithm>
#include <functional>
#include <utility>

#include "front/analyzer.h"

namespace yulang::front {
using define::AccessAST;
using define::ASTPtr;
using define::ASTPtrList;
using define::BaseAST;
using define::BinaryAST;
using define::BlockAST;
using define::CastAST;
using define::ControlAST;
using define::DeclareAST;
using define::EnumAST;
using define::ForInAST;
using define::FunCallAST;
using define::FuncType;
using define::FuncTypeAST;
using define::FunDefAST;
using define::IdAST;
using define::IfAST;
using define::ImportAST;
using define::ImportKind;
using define::IndexAST;
using define::IsExtern;
using define::IsInline;
using define::IsPublic;
using define::MakeVoid;
using define::Property;
using define::SourceModulePtr;
using define::StructAST;
using define::StructType;
using define::TypeAliasAST;
using define::TypePairList;
using define::TypePtr;
using define::TypePtrList;
using define::UnaryAST;
using define::ValInitAST;
using define::VarLetDefAST;
using define::VarLetElemAST;
using define::WhenAST;
using define::WhenElemAST;
using define::WhileAST;

namespace {
std::string Encode(const std::string &s) {
  return std::to_string(s.size()) + s;
}
Property GetProperty(BaseAST &ast) {
  if (auto const *a = dynamic_cast<FunDefAST *>(&ast)) return a->prop();
  if (auto const *a = dynamic_cast<DeclareAST *>(&ast)) return a->prop();
  if (auto const *a = dynamic_cast<TypeAliasAST *>(&ast)) return a->prop();
  if (auto const *a = dynamic_cast<StructAST *>(&ast)) return a->prop();
  if (auto const *a = dynamic_cast<EnumAST *>(&ast)) return a->prop();
  return Property::None;
}
std::string DefinitionName(BaseAST &ast) {
  if (auto const *a = dynamic_cast<FunDefAST *>(&ast)) return a->id();
  if (auto const *a = dynamic_cast<DeclareAST *>(&ast)) return a->id();
  if (auto const *a = dynamic_cast<TypeAliasAST *>(&ast)) return a->id();
  if (auto const *a = dynamic_cast<StructAST *>(&ast)) return a->id();
  if (auto const *a = dynamic_cast<EnumAST *>(&ast)) return a->id();
  if (auto const *a = dynamic_cast<VarLetElemAST *>(&ast)) return a->id();
  return {};
}
}  // namespace

xstl::Guard Analyzer::EnterModule(ModuleState &module) {
  auto *previous = current_module_;
  auto const symbols = symbols_;
  auto const types = user_types_;
  auto const funcs = funcs_;
  current_module_ = &module;
  symbols_ = module.symbols;
  user_types_ = module.types;
  funcs_ = module.funcs;
  return xstl::Guard([this, previous, symbols, types, funcs] {
    current_module_ = previous;
    symbols_ = symbols;
    user_types_ = types;
    funcs_ = funcs;
  });
}

std::string Analyzer::MangleSymbol(const std::string &name, char kind) const {
  const auto module =
      current_module_ ? current_module_->source->id : "<anonymous>";
  return "_Y" + std::string(1, kind) + Encode(module) + Encode(name);
}

void Analyzer::CollectDefinition(ModuleState &module, BaseAST &ast,
                                 Property prop) {
  const auto name = DefinitionName(ast);
  if (name.empty()) return;
  auto def = std::make_unique<Definition>();
  def->ast = &ast;
  def->owner = &module;
  def->prop = prop;
  def->name = name;
  def->is_type = dynamic_cast<StructAST *>(&ast) ||
                 dynamic_cast<EnumAST *>(&ast) ||
                 dynamic_cast<TypeAliasAST *>(&ast);
  def->is_function = dynamic_cast<FunDefAST *>(&ast);
  if (const auto *declaration = dynamic_cast<DeclareAST *>(&ast))
    def->is_function = dynamic_cast<FuncTypeAST *>(declaration->type().get());
  auto &same = module.definitions[name];
  for (const auto *other : same)
    if (def->is_type == other->is_type && !def->is_function &&
        !other->is_function)
      ast.logger().LogError("definition name has already been used", name);
  same.push_back(def.get());
  definition_by_ast_[&ast] = def.get();
  if (auto *structure = dynamic_cast<StructAST *>(&ast)) {
    const auto type = std::make_shared<StructType>(TypePairList{}, name, false);
    type->SetIdentity(MangleSymbol(name, 'T'), module.source->id,
                      IsPublic(prop));
    structure->set_ast_type(type);
    module.types->AddItem(name, type);
  }
  definitions_.push_back(std::move(def));
}

void Analyzer::CollectModule(ModuleState &module) {
  const auto scope = EnterModule(module);
  for (const auto &ast : module.source->defs) {
    if (auto *vars = dynamic_cast<VarLetDefAST *>(ast.get())) {
      for (const auto &var : vars->defs())
        CollectDefinition(module, *var, vars->prop());
      vars->set_ast_type(MakeVoid());
    } else {
      CollectDefinition(module, *ast, GetProperty(*ast));
    }
  }
}

void Analyzer::AnalyzeBodies(ModuleState &module) {
  const auto scope = EnterModule(module);
  for (const auto &ast : module.source->defs) {
    auto *function = dynamic_cast<FunDefAST *>(ast.get());
    if (!function) continue;
    current_definition_ = definition_by_ast_.at(function);
    if (current_definition_->state == 2) {
      const auto errors = Logger::error_num();
      if (AnalyzeExpr(*function) && Logger::error_num() == errors)
        function->Eval(eval_);
    }
    current_definition_ = nullptr;
  }
}

void Analyzer::PromoteInlineSymbols() {
  references_.clear();
  for (const auto &definition : definitions_) {
    if (const auto *function = dynamic_cast<FunDefAST *>(definition->ast);
        function && function->body()) {
      current_definition_ = definition.get();
      CollectRuntimeReferences(*function->body());
    }
  }
  current_definition_ = nullptr;
  std::unordered_set<BaseAST *> scanned;
  std::function<void(BaseAST *)> promote = [&](BaseAST *body) {
    if (!scanned.insert(body).second) return;
    for (auto *dependency : references_[body]) {
      dependency->set_abi_reachable(true);
      if (auto *f = dynamic_cast<FunDefAST *>(dependency);
          f && IsInline(f->prop()))
        promote(f);
    }
  };
  for (const auto &definition : definitions_)
    if (!definition->owner->source->anonymous && definition->is_function &&
        IsPublic(definition->prop) && IsInline(definition->prop))
      promote(definition->ast);
}

bool Analyzer::AnalyzeModules(const std::vector<SourceModulePtr> &modules,
                              const SourceModulePtr &root) {
  for (const auto &source : modules) {
    auto state = std::make_unique<ModuleState>();
    state->source = source;
    modules_.emplace(source->id, std::move(state));
  }
  for (const auto &source : modules) CollectModule(*modules_.at(source->id));
  EnsureModule(*modules_.at(root->id));
  for (const auto &source : modules) EnsureModule(*modules_.at(source->id));
  // Bodies see complete signatures and layouts. Emission retains source order.
  for (const auto &source : modules) AnalyzeBodies(*modules_.at(source->id));
  if (Logger::error_num()) return false;
  PromoteInlineSymbols();
  return true;
}

bool Analyzer::EnsureModule(ModuleState &module) {
  if (module.state == 2) return true;
  if (module.state == 3) return false;
  if (module.state == 1) {
    Logger::LogRawError("cyclic module import");
    return false;
  }
  module.state = 1;
  const auto scope = EnterModule(module);
  bool valid = true;
  for (const auto &ast : module.source->defs)
    if (auto *import = dynamic_cast<ImportAST *>(ast.get()))
      valid = BindImport(*import) && valid;
  for (const auto &ast : module.source->defs) {
    if (auto const *vars = dynamic_cast<VarLetDefAST *>(ast.get())) {
      for (const auto &var : vars->defs())
        valid = EnsureDefinition(*definition_by_ast_.at(var.get())) && valid;
    } else if (auto const it = definition_by_ast_.find(ast.get());
               it != definition_by_ast_.end()) {
      valid = EnsureDefinition(*it->second) && valid;
    }
  }
  module.state = valid ? 2 : 3;
  return valid;
}

bool Analyzer::EnsureDefinition(Definition &definition) {
  if (definition.state == 2) return true;
  if (definition.state == 3) return false;
  if (definition.state == 1) {
    definition.ast->logger().LogError("cyclic definition or structure layout",
                                      definition.name);
    return false;
  }
  definition.state = 1;
  const auto module = EnterModule(*definition.owner);
  const auto values = eval_.GlobalEnv();
  auto *old_definition = std::exchange(current_definition_, &definition);
  auto old_prop = std::exchange(last_prop_, definition.prop);
  auto old_ret = std::exchange(cur_ret_, nullptr);
  auto old_expected = std::exchange(expected_, nullptr);
  auto old_indirection = std::exchange(type_indirection_, 0);
  auto owners = std::move(lookup_owners_);
  const xstl::Guard restore([&, this] {
    current_definition_ = old_definition;
    last_prop_ = old_prop;
    cur_ret_ = old_ret;
    expected_ = old_expected;
    type_indirection_ = old_indirection;
    lookup_owners_ = std::move(owners);
  });
  const auto errors = Logger::error_num();
  TypePtr type;
  if (auto *function = dynamic_cast<FunDefAST *>(definition.ast)) {
    type = DeclareFunction(*function);
    definition.symbol = function->id();
  } else {
    type = AnalyzeExpr(*definition.ast);
    if (type && Logger::error_num() == errors) definition.ast->Eval(eval_);
    if (auto const *var = dynamic_cast<VarLetElemAST *>(definition.ast))
      definition.symbol = var->id();
    if (auto const *decl = dynamic_cast<DeclareAST *>(definition.ast)) {
      definition.symbol = decl->id();
      if (decl->type()->ast_type())
        definition.is_function = decl->type()->ast_type()->IsFunction();
    }
  }
  const bool valid = type && Logger::error_num() == errors;
  definition.state = valid ? 2 : 3;
  return valid && PublishDefinition(definition);
}

bool Analyzer::PublishDefinition(Definition &definition) {
  if (!definition.symbol.empty()) {
    auto const symbol_type = symbols_->GetItem(definition.symbol);
    const auto existing = canonical_types_.find(definition.symbol);
    if (existing != canonical_types_.end() &&
        !existing->second->IsIdentical(symbol_type)) {
      definition.ast->logger().LogError("conflicting external declarations",
                                        definition.name);
      definition.state = 3;
      return false;
    }
    canonical_types_[definition.symbol] = symbol_type;
    canonical_definitions_[definition.symbol] = &definition;
    if (!definition.is_function)
      definition.owner->value_ids[definition.name] = definition.symbol;
  }
  if (IsPublic(definition.prop)) {
    TypePtr exposed;
    if (auto const *alias = dynamic_cast<TypeAliasAST *>(definition.ast))
      exposed = alias->type()->ast_type();
    else if (auto const *decl = dynamic_cast<DeclareAST *>(definition.ast))
      exposed = decl->type()->ast_type();
    else if (!dynamic_cast<StructAST *>(definition.ast) &&
             !dynamic_cast<EnumAST *>(definition.ast))
      exposed = definition.ast->ast_type();
    if (exposed && !PublicType(exposed)) {
      definition.ast->logger().LogError("public API exposes a private type",
                                        definition.name);
      definition.state = 3;
      return false;
    }
  }
  return true;
}

bool Analyzer::EnsureName(const std::string &name, bool types) {
  if (!current_module_) return true;
  bool valid = true;
  for (auto *definition : LookupDefinitions(name)) {
    if (definition->is_type != types) continue;
    if (types && type_indirection_ &&
        dynamic_cast<StructAST *>(definition->ast))
      continue;
    valid = EnsureDefinition(*definition) && valid;
  }
  return valid;
}

std::vector<Analyzer::Definition *> Analyzer::LookupDefinitions(
    const std::string &name) {
  if (!current_module_) return {};
  const auto dot = name.find('.');
  if (dot != std::string::npos) {
    auto const ns = current_module_->namespaces.find(name.substr(0, dot));
    if (ns == current_module_->namespaces.end()) return {};
    auto const it = ns->second->definitions.find(name.substr(dot + 1));
    if (it == ns->second->definitions.end()) return {};
    std::vector<Definition *> found;
    for (auto *def : it->second)
      if (IsPublic(def->prop)) found.push_back(def);
    return found;
  }
  if (auto const it = current_module_->definitions.find(name);
      it != current_module_->definitions.end())
    return it->second;
  if (auto const it = current_module_->imports.find(name);
      it != current_module_->imports.end())
    return it->second;
  return {};
}

bool Analyzer::BindName(const Logger &log, ModuleState &from,
                        const std::string &name, const std::string &alias) {
  auto const it = from.definitions.find(name);
  std::vector<Definition *> found;
  if (it != from.definitions.end())
    for (auto *def : it->second)
      if (IsPublic(def->prop)) found.push_back(def);
  if (found.empty()) {
    log.LogError("imported definition is not public or does not exist", name);
    return false;
  }
  if (current_module_->namespaces.count(alias) ||
      current_module_->definitions.count(alias) ||
      current_module_->imports.count(alias)) {
    log.LogError("import binding conflicts with an existing name", alias);
    return false;
  }
  if (std::any_of(found.begin(), found.end(),
                  [](const auto *def) { return def->state != 2; }))
    return false;
  current_module_->imports[alias] = found;
  for (auto const *def : found) {
    if (def->is_type) {
      auto const type = from.types->GetItem(name);
      current_module_->types->AddItem(alias, type);
      if (type->IsEnum())
        current_module_->symbols->AddItem(alias, type->GetValueType(true));
    } else if (def->is_function) {
      auto &overloads = current_module_->funcs->AccessItem(alias);
      if (!overloads) overloads.emplace();
      overloads->push_back(def->symbol);
    } else {
      current_module_->symbols->AddItem(alias,
                                        canonical_types_.at(def->symbol));
      current_module_->value_ids[alias] = def->symbol;
    }
  }
  return true;
}

bool Analyzer::BindImport(ImportAST &ast) {
  auto &target = *modules_.at(ast.module()->id);
  bool valid = EnsureModule(target);
  if (ast.alternative_module()) {
    auto &alternative = *modules_.at(ast.alternative_module()->id);
    if (auto const it = alternative.definitions.find(ast.alternative_member());
        it != alternative.definitions.end() &&
        std::any_of(it->second.begin(), it->second.end(),
                    [](auto *d) { return IsPublic(d->prop); })) {
      ast.logger().LogError(
          "ambiguous import: module and definition have the same path");
      return false;
    }
  }
  if (ast.kind() == ImportKind::Namespace) {
    const auto &name = ast.bindings().front().alias;
    if (current_module_->definitions.count(name) ||
        current_module_->imports.count(name) ||
        !current_module_->namespaces.emplace(name, &target).second) {
      ast.logger().LogError("import binding conflicts with an existing name",
                            name);
      return false;
    }
  } else if (ast.kind() == ImportKind::Wildcard) {
    for (const auto &[name, definitions] : target.definitions)
      if (std::any_of(definitions.begin(), definitions.end(),
                      [](auto *d) { return IsPublic(d->prop); }))
        valid = BindName(ast.logger(), target, name, name) && valid;
  } else {
    for (const auto &binding : ast.bindings()) {
      const auto definitions = target.definitions.find(binding.name);
      const bool accessible =
          definitions != target.definitions.end() &&
          std::any_of(definitions->second.begin(), definitions->second.end(),
                      [](auto *def) { return IsPublic(def->prop); });
      if (binding.module_alternative && accessible) {
        ast.logger().LogError(
            "ambiguous import: module and definition have the same path",
            binding.name);
        valid = false;
      } else {
        valid = BindName(ast.logger(), target, binding.name, binding.alias) &&
                valid;
      }
    }
  }
  ast.set_ast_type(MakeVoid());
  return valid;
}

TypePtr Analyzer::SymbolType(const std::string &name) const {
  if (auto type = symbols_->GetItem(name)) return type;
  if (auto const it = canonical_types_.find(name); it != canonical_types_.end())
    return it->second;
  return nullptr;
}

std::vector<std::string> Analyzer::VisibleCallNames(const std::string &name) {
  std::vector<std::string> result;
  if (name.find('.') != std::string::npos) {
    for (const auto *def : LookupDefinitions(name))
      if (def->is_function) result.push_back(def->symbol);
    return result;
  }
  for (auto env = funcs_; env; env = env->outer())
    if (auto overloads = env->GetItem(name, false))
      result.insert(result.end(), overloads->begin(), overloads->end());
  if (const auto type = symbols_->GetItem(name); type && type->IsFunction()) {
    const auto found = current_module_->value_ids.find(name);
    result.push_back(found == current_module_->value_ids.end() ? name
                                                               : found->second);
  }
  return result;
}

std::vector<std::pair<std::string, TypePtr>> Analyzer::CallCandidates(
    const std::string &name) {
  std::vector<std::pair<std::string, TypePtr>> result;
  for (auto *def : LookupDefinitions(name)) {
    // A value being initialized must not hide the separate function namespace.
    if (def->is_type || (def == current_definition_ && !def->is_function))
      continue;
    if (!EnsureDefinition(*def)) return result;
  }
  auto names = VisibleCallNames(name);
  for (auto *owner : lookup_owners_) {
    const auto it = owner->definitions.find(name);
    if (it == owner->definitions.end()) continue;
    for (auto *def : it->second) {
      if (!def->is_function ||
          (owner != current_module_ && !IsPublic(def->prop)))
        continue;
      if (EnsureDefinition(*def)) names.push_back(def->symbol);
    }
  }
  std::unordered_set<std::string> seen;
  for (const auto &name : names) {
    const auto type = SymbolType(name);
    if (seen.insert(name).second && type && type->IsFunction())
      result.emplace_back(name, type);
  }
  return result;
}

void Analyzer::SetLookupOwners(const TypePtrList &args, bool all) {
  lookup_owners_.clear();
  for (const auto &arg : args) {
    auto type = arg;
    if (type) {
      while (type->IsPointer() || type->IsReference())
        type = type->GetDerefedType();
      auto const found = modules_.find(type->GetOwnerModule());
      if (found != modules_.end())
        lookup_owners_.push_back(found->second.get());
    }
    if (!all) break;
  }
}

bool Analyzer::QualifiedName(BaseAST &ast, std::string &name) const {
  if (auto const *id = dynamic_cast<IdAST *>(&ast)) {
    if (!current_module_ || !current_module_->namespaces.count(id->id()))
      return false;
    // A local binding shadows an imported namespace at the point of use.
    for (auto env = symbols_; env && !env->is_root(); env = env->outer())
      if (env->GetItem(id->id(), false)) return false;
    name = id->id();
    return true;
  }
  if (auto const *access = dynamic_cast<AccessAST *>(&ast)) {
    if (!QualifiedName(*access->expr(), name)) return false;
    name += "." + access->id();
    return true;
  }
  return false;
}

TypePtr Analyzer::ResolveValue(const Logger &log, const std::string &name,
                               std::string &resolved) {
  for (auto env = symbols_; env && !env->is_root(); env = env->outer()) {
    if (auto type = env->GetItem(name, false)) {
      resolved = name;
      return type;
    }
  }
  if (!EnsureName(name, false)) return nullptr;
  if (!EnsureName(name, true)) return nullptr;
  const auto defs = LookupDefinitions(name);
  for (const auto *def : defs) {
    if (dynamic_cast<EnumAST *>(def->ast)) {
      resolved.clear();
      return def->owner->types->GetItem(def->name)->GetValueType(true);
    }
    if (!def->is_type && !def->is_function) {
      resolved = def->symbol;
      RecordReference(resolved);
      return SymbolType(resolved);
    }
  }
  if (auto type = symbols_->GetItem(name)) {
    resolved = name;
    if (current_module_) {
      auto const it = current_module_->value_ids.find(name);
      if (it != current_module_->value_ids.end()) resolved = it->second;
    }
    RecordReference(resolved);
    return type;
  }
  const auto candidates = CallCandidates(name);
  if (candidates.size() == 1) {
    resolved = candidates.front().first;
    RecordReference(resolved);
    return candidates.front().second;
  }
  log.LogError(candidates.size() > 1
                   ? "ambiguous function value"
                   : "identifier has not been defined or is not public",
               name);
  return nullptr;
}

bool Analyzer::PublicType(const TypePtr &type) const {
  if (type->IsReference() || type->IsPointer() || type->IsArray())
    return PublicType(type->GetDerefedType());
  if (type->IsStruct() || type->IsEnum()) return type->IsPublicType();
  if (type->IsFunction()) {
    const auto args = type->GetArgsType();
    if (!args) return false;
    for (const auto &arg : *args)
      if (!PublicType(arg)) return false;
    return PublicType(type->GetReturnType(*args));
  }
  return true;
}

bool Analyzer::CanDefaultInitialize(const TypePtr &type) const {
  if (type->IsReference() || type->IsPointer()) return true;
  if (type->IsArray())
    return type->GetLength() == 0 ||
           CanDefaultInitialize(type->GetDerefedType());
  if (!type->IsStruct()) return true;
  const bool owned = type->GetOwnerModule() == current_module_->source->id;
  for (std::size_t i = 0; i < type->GetLength(); ++i) {
    if (!type->IsFieldPublic(i)) {
      if (!owned) return false;
      continue;
    }
    if (!CanDefaultInitialize(type->GetElem(i))) return false;
  }
  return true;
}

bool Analyzer::CheckAggregateVisibility(const TypePtr &type,
                                        std::size_t provided,
                                        const Logger &log) const {
  bool allowed = true;
  if (type->IsArray()) {
    allowed = provided == type->GetLength() ||
              CanDefaultInitialize(type->GetDerefedType());
  } else {
    const bool owned = type->GetOwnerModule() == current_module_->source->id;
    for (std::size_t i = 0; i < type->GetLength(); ++i) {
      if (!type->IsFieldPublic(i)) {
        allowed &= owned;
      } else if (i >= provided) {
        allowed &= CanDefaultInitialize(type->GetElem(i));
      }
    }
  }
  if (!allowed)
    log.LogError("cannot initialize a structure with private fields");
  return allowed;
}

bool Analyzer::CheckCallableBinding(const BaseAST &callee) const {
  const auto *id = dynamic_cast<const IdAST *>(&callee);
  if (!id) return true;
  for (auto env = symbols_; env && !env->is_root(); env = env->outer()) {
    if (const auto local = env->GetItem(id->id(), false)) {
      if (local->IsFunction()) return true;
      callee.logger().LogError("calling a non-function", id->id());
      return false;
    }
  }
  return true;
}

void Analyzer::RecordReference(const std::string &symbol) {
  if (!current_definition_) return;
  auto const found = canonical_definitions_.find(symbol);
  if (found != canonical_definitions_.end())
    references_[current_definition_->ast].insert(found->second->ast);
}

// Scan the evaluated body: layout constants and folded values do not require
// linker symbols. Calls introduced by operator and iterator lowering do.
bool Analyzer::CollectExpressionReferences(BaseAST &ast) {
  auto const visit = [this](const ASTPtr &child) {
    if (child) CollectRuntimeReferences(*child);
  };
  auto const list = [this](const ASTPtrList &children) {
    for (const auto &child : children) CollectRuntimeReferences(*child);
  };
  if (auto const *node = dynamic_cast<IdAST *>(&ast)) {
    RecordReference(node->id());
  } else if (auto const *node = dynamic_cast<AccessAST *>(&ast)) {
    if (!node->resolved_id().empty())
      RecordReference(node->resolved_id());
    else
      visit(node->expr());
  } else if (auto const *node = dynamic_cast<FunCallAST *>(&ast)) {
    visit(node->expr());
    list(node->args());
  } else if (auto const *node = dynamic_cast<BinaryAST *>(&ast)) {
    if (node->op_func_id()) RecordReference(*node->op_func_id());
    visit(node->lhs());
    visit(node->rhs());
  } else if (auto const *node = dynamic_cast<UnaryAST *>(&ast)) {
    if (node->op() == UnaryAST::UnaryOp::SizeOf) return true;
    if (node->op_func_id()) RecordReference(*node->op_func_id());
    visit(node->opr());
  } else if (auto const *node = dynamic_cast<IndexAST *>(&ast)) {
    visit(node->expr());
    visit(node->index());
  } else if (auto const *node = dynamic_cast<CastAST *>(&ast)) {
    visit(node->expr());
  } else if (auto const *node = dynamic_cast<ValInitAST *>(&ast)) {
    list(node->elems());
  } else {
    return false;
  }
  return true;
}

void Analyzer::CollectRuntimeReferences(BaseAST &ast) {
  if (CollectExpressionReferences(ast)) return;
  const auto visit = [this](const ASTPtr &child) {
    if (child) CollectRuntimeReferences(*child);
  };
  const auto list = [this](const ASTPtrList &children) {
    for (const auto &child : children) CollectRuntimeReferences(*child);
  };
  if (auto const *node = dynamic_cast<BlockAST *>(&ast)) {
    list(node->stmts());
  } else if (auto const *node = dynamic_cast<VarLetDefAST *>(&ast)) {
    list(node->defs());
  } else if (auto const *node = dynamic_cast<VarLetElemAST *>(&ast)) {
    visit(node->init());
  } else if (auto const *node = dynamic_cast<IfAST *>(&ast)) {
    visit(node->cond());
    visit(node->then());
    visit(node->else_then());
  } else if (auto const *node = dynamic_cast<WhenAST *>(&ast)) {
    visit(node->expr());
    list(node->elems());
    visit(node->else_then());
  } else if (auto const *node = dynamic_cast<WhenElemAST *>(&ast)) {
    list(node->conds());
    visit(node->body());
  } else if (auto const *node = dynamic_cast<WhileAST *>(&ast)) {
    visit(node->cond());
    visit(node->body());
  } else if (auto const *node = dynamic_cast<ForInAST *>(&ast)) {
    RecordReference(node->next_id());
    RecordReference(node->last_id());
    visit(node->expr());
    visit(node->body());
  } else if (auto const *node = dynamic_cast<ControlAST *>(&ast)) {
    visit(node->expr());
  }
}

TypePtr Analyzer::DeclareFunction(FunDefAST &ast) {
  const auto args_env = NewEnv();
  const auto eval_env = eval_.NewEnv();
  TypePtrList args;
  for (const auto &arg : ast.args()) {
    auto const type = AnalyzeExpr(*arg);
    if (!type) return nullptr;
    args.push_back(type);
  }
  auto ret = ast.type() ? AnalyzeExpr(*ast.type()) : MakeVoid();
  if (!ret) return nullptr;
  if (!ret->IsReference() && !ret->IsRightValue())
    ret = ret->GetValueType(true);
  const auto original = ast.id();
  if (original == ".") {
    ast.logger().LogError("access operator cannot be overloaded");
    return nullptr;
  }
  const auto id =
      IsExtern(ast.prop()) ? original : MangleFuncName(original, args);
  if (symbols_->outer()->GetItem(id, false)) {
    ast.logger().LogError("function has already been defined", original);
    return nullptr;
  }
  auto type = std::make_shared<FuncType>(std::move(args), std::move(ret), true);
  ast.set_id(id);
  ast.set_ast_type(type);
  symbols_->outer()->AddItem(id, type);
  auto &overloads = funcs_->outer()->AccessItem(original);
  if (!overloads) overloads.emplace();
  overloads->push_back(id);
  return type;
}

}  // namespace yulang::front
