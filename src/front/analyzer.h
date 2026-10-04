#ifndef YULANG_FRONT_ANALYZER_H_
#define YULANG_FRONT_ANALYZER_H_

#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include "define/ast.h"
#include "define/symbol.h"
#include "define/token.h"
#include "define/type.h"
#include "front/eval.h"
#include "front/logger.h"
#include "xstl/guard.h"

namespace yulang::front {

class Analyzer {
 public:
  explicit Analyzer(Evaluator &eval) : eval_(eval) { Reset(); }

  void Reset() {
    symbols_ = define::MakeEnv();
    user_types_ = define::MakeEnv();
    funcs_ = define::MakeFuncMap();
    in_loop_ = 0;
    cur_ret_.reset();
    expected_.reset();
    last_prop_ = define::Property::None;
    modules_.clear();
    definitions_.clear();
    definition_by_ast_.clear();
    canonical_types_.clear();
    canonical_definitions_.clear();
    references_.clear();
    lookup_owners_.clear();
    current_module_ = nullptr;
    current_definition_ = nullptr;
    type_indirection_ = 0;
  }

  bool AnalyzeModules(const std::vector<define::SourceModulePtr> &modules,
                      const define::SourceModulePtr &root);

  // Context applies only to this expression and explicitly selected children.
  define::TypePtr AnalyzeExpr(define::BaseAST &ast,
                              define::TypePtr expected = {});

  define::TypePtr AnalyzeOn(define::VarLetDefAST &ast);
  define::TypePtr AnalyzeOn(define::FunDefAST &ast);
  define::TypePtr AnalyzeOn(define::DeclareAST &ast);
  define::TypePtr AnalyzeOn(define::TypeAliasAST &ast);
  define::TypePtr AnalyzeOn(define::StructAST &ast);
  define::TypePtr AnalyzeOn(define::EnumAST &ast);
  define::TypePtr AnalyzeOn(define::ImportAST &ast);
  define::TypePtr AnalyzeOn(define::VarLetElemAST &ast);
  define::TypePtr AnalyzeOn(define::ArgElemAST &ast);
  define::TypePtr AnalyzeOn(define::StructElemAST &ast);
  define::TypePtr AnalyzeOn(define::EnumElemAST &ast);
  define::TypePtr AnalyzeOn(define::BlockAST &ast);
  define::TypePtr AnalyzeOn(define::IfAST &ast);
  define::TypePtr AnalyzeOn(define::WhenAST &ast);
  define::TypePtr AnalyzeOn(define::WhileAST &ast);
  define::TypePtr AnalyzeOn(define::ForInAST &ast);
  static define::TypePtr AnalyzeOn(define::AsmAST &ast);
  define::TypePtr AnalyzeOn(define::ControlAST &ast);
  define::TypePtr AnalyzeOn(define::WhenElemAST &ast);
  define::TypePtr AnalyzeOn(define::BinaryAST &ast);
  define::TypePtr AnalyzeOn(define::AccessAST &ast);
  define::TypePtr AnalyzeOn(define::CastAST &ast);
  define::TypePtr AnalyzeOn(define::UnaryAST &ast);
  define::TypePtr AnalyzeOn(define::IndexAST &ast);
  define::TypePtr AnalyzeOn(define::FunCallAST &ast);
  define::TypePtr AnalyzeOn(define::IntAST &ast);
  static define::TypePtr AnalyzeOn(define::FloatAST &ast);
  static define::TypePtr AnalyzeOn(define::CharAST &ast);
  define::TypePtr AnalyzeOn(define::IdAST &ast);
  static define::TypePtr AnalyzeOn(define::StringAST &ast);
  static define::TypePtr AnalyzeOn(define::BoolAST &ast);
  define::TypePtr AnalyzeOn(define::NullAST &ast);
  define::TypePtr AnalyzeOn(define::ValInitAST &ast);
  static define::TypePtr AnalyzeOn(define::PrimTypeAST &ast);
  define::TypePtr AnalyzeOn(define::UserTypeAST &ast);
  define::TypePtr AnalyzeOn(define::FuncTypeAST &ast);
  define::TypePtr AnalyzeOn(define::VolaTypeAST &ast);
  define::TypePtr AnalyzeOn(define::ArrayTypeAST &ast);
  define::TypePtr AnalyzeOn(define::PointerTypeAST &ast);
  define::TypePtr AnalyzeOn(define::RefTypeAST &ast);

 private:
  using IdSetter = std::function<void(const std::string &)>;

  // switch to new environment
  xstl::Guard NewEnv();
  // perform name mangling
  std::string MangleFuncName(const std::string &id,
                             const define::TypePtrList &args);
  // A callable local binding takes precedence over named overloads.
  [[nodiscard]] define::TypePtr FindLocalFunction(const std::string &id) const;
  // check and add user type
  bool AddUserType(const Logger &log, const std::string &id,
                   const define::TypePtr &type);
  // find function type in current environment
  // call 'id_setter' using function name
  // print error message if not found
  define::TypePtr FindFuncType(const Logger &log, const std::string &id,
                               const define::TypePtrList &args,
                               const IdSetter &id_setter);
  // Find a unique literal-compatible signature without modifying candidates.
  define::TypePtr ResolveCall(const Logger &log, const std::string &id,
                              define::TypePtrList &args,
                              const std::vector<define::BaseAST *> &exprs,
                              const IdSetter &id_setter);
  [[nodiscard]] define::TypePtrList GetCallCandidates(const std::string &id);
  bool AnalyzeInferredArguments(define::TypePtrList &args,
                                const std::vector<define::BaseAST *> &exprs,
                                const define::TypePtrList &candidates);
  // check if is valid initialization
  // type: variable type, init: initializer type
  static bool CheckInit(const Logger &log, const define::TypePtr &type,
                        const define::TypePtr &init);
  // check if is valid initialization (with identifier)
  static bool CheckInit(const Logger &log, const define::TypePtr &type,
                        const define::TypePtr &init, std::string_view id);
  // check if is valid operator overloading
  // if so, return function's return type, otherwise log error
  std::optional<define::TypePtr> CheckOpOverload(
      const Logger &log, const std::string &op_name, define::TypePtrList &args,
      const IdSetter &id_setter, const std::vector<define::BaseAST *> &exprs);

  struct ModuleState;
  struct Definition {
    define::BaseAST *ast{};
    ModuleState *owner{};
    define::Property prop{};
    std::string name;
    std::string symbol;
    int state{};
    bool is_type{};
    bool is_function{};
  };
  struct ModuleState {
    define::SourceModulePtr source;
    define::EnvPtr symbols = define::MakeEnv();
    define::EnvPtr types = define::MakeEnv();
    define::FuncMapPtr funcs = define::MakeFuncMap();
    std::unordered_map<std::string, std::vector<Definition *>> definitions;
    std::unordered_map<std::string, std::vector<Definition *>> imports;
    std::unordered_map<std::string, ModuleState *> namespaces;
    std::unordered_map<std::string, std::string> value_ids;
    int state{};
    std::size_t local_type_id{};
  };
  std::unordered_map<std::string, std::unique_ptr<ModuleState>> modules_;
  std::vector<std::unique_ptr<Definition>> definitions_;
  std::unordered_map<define::BaseAST *, Definition *> definition_by_ast_;
  std::unordered_map<std::string, define::TypePtr> canonical_types_;
  std::unordered_map<std::string, Definition *> canonical_definitions_;
  std::unordered_map<define::BaseAST *, std::unordered_set<define::BaseAST *>>
      references_;
  ModuleState *current_module_{};
  Definition *current_definition_{};
  std::vector<ModuleState *> lookup_owners_;
  unsigned type_indirection_{};

  xstl::Guard EnterModule(ModuleState &module);
  void CollectDefinition(ModuleState &module, define::BaseAST &ast,
                         define::Property prop);
  void CollectModule(ModuleState &module);
  void AnalyzeBodies(ModuleState &module);
  void PromoteInlineSymbols();
  bool PublishDefinition(Definition &definition);
  std::vector<std::string> VisibleCallNames(const std::string &name);
  bool AnalyzeCallArguments(const define::ASTPtrList &arguments,
                            define::TypePtrList &args,
                            std::vector<define::BaseAST *> &exprs,
                            bool &needs_context);
  bool LowerDotCall(define::FunCallAST &ast);
  bool CollectExpressionReferences(define::BaseAST &ast);
  bool EnsureModule(ModuleState &module);
  bool EnsureDefinition(Definition &definition);
  bool EnsureName(const std::string &name, bool types);
  bool BindImport(define::ImportAST &ast);
  bool BindName(const front::Logger &log, ModuleState &from,
                const std::string &name, const std::string &alias);
  std::vector<Definition *> LookupDefinitions(const std::string &name);
  std::vector<std::pair<std::string, define::TypePtr>> CallCandidates(
      const std::string &name);
  [[nodiscard]] define::TypePtr SymbolType(const std::string &name) const;
  define::TypePtr DeclareFunction(define::FunDefAST &ast);
  define::TypePtr ResolveValue(const front::Logger &log,
                               const std::string &name, std::string &resolved);
  bool QualifiedName(define::BaseAST &ast, std::string &name) const;
  [[nodiscard]] bool PublicType(const define::TypePtr &type) const;
  [[nodiscard]] bool CanDefaultInitialize(const define::TypePtr &type) const;
  [[nodiscard]] bool CheckAggregateVisibility(const define::TypePtr &type,
                                              std::size_t provided,
                                              const Logger &log) const;
  [[nodiscard]] bool CheckCallableBinding(const define::BaseAST &callee) const;
  void RecordReference(const std::string &symbol);
  void CollectRuntimeReferences(define::BaseAST &ast);
  void SetLookupOwners(const define::TypePtrList &args, bool all);
  [[nodiscard]] std::string MangleSymbol(const std::string &name,
                                         char kind) const;

  // evaluator
  // This service is bound to its owner for its entire lifetime.
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  Evaluator &eval_;
  // symbol tables & user defined types (structs, enums, aliases)
  define::EnvPtr symbols_, user_types_;
  // Overload names in each lexical scope, including declared functions.
  define::FuncMapPtr funcs_;
  // used when analyzing functions
  define::TypePtr cur_ret_;
  define::TypePtr expected_;
  // used when analyzing structures
  std::string_view last_struct_elem_name_;
  // used when analyzing enumerations
  define::TypePtr last_enum_type_;
  std::string last_enum_elem_name_;
  // used when analyzing var/let definitions
  define::Property last_prop_{define::Property::None};
  // used when analyzing 'when' statements
  define::TypePtr last_when_expr_type_;
  // used when analyzing while loop & for loop
  std::uint64_t in_loop_{};
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_ANALYZER_H_
