#ifndef YULANG_DEFINE_AST_H_
#define YULANG_DEFINE_AST_H_

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "define/symbol.h"
#include "define/token.h"
#include "define/type.h"
#include "front/logger.h"
#include "mid/usedef.h"

// forward declarations for visitor pattern
namespace yulang {
namespace front {
class Analyzer;
class Evaluator;
}  // namespace front

namespace mid {
class IRBuilder;
}  // namespace mid
}  // namespace yulang

namespace yulang::define {

// property of statements
enum class Property : std::uint8_t {
  None = 0,
  Public = 1,
  Extern = 2,
  Inline = 4,
};

constexpr Property operator|(Property lhs, Property rhs) {
  return static_cast<Property>(static_cast<std::uint8_t>(lhs) |
                               static_cast<std::uint8_t>(rhs));
}

constexpr bool HasProperty(Property prop, Property flag) {
  return (static_cast<std::uint8_t>(prop) & static_cast<std::uint8_t>(flag)) !=
         0;
}

constexpr bool IsPublic(Property prop) {
  return HasProperty(prop, Property::Public);
}
constexpr bool IsExtern(Property prop) {
  return HasProperty(prop, Property::Extern);
}
constexpr bool IsInline(Property prop) {
  return HasProperty(prop, Property::Inline);
}

// definition of base class of all ASTs
class BaseAST {
 public:
  BaseAST() = default;
  BaseAST(const BaseAST &) = default;
  BaseAST &operator=(const BaseAST &) = default;
  BaseAST(BaseAST &&) = default;
  BaseAST &operator=(BaseAST &&) = default;

  virtual ~BaseAST() = default;

  // return true if current AST is an identifier
  [[nodiscard]] virtual bool IsId() const = 0;
  // return true if current AST is a literal value
  [[nodiscard]] virtual bool IsLiteral() const = 0;

  // dump the content of AST to output stream
  virtual void Dump(std::ostream &os) const = 0;
  // run sematic analysis on current AST
  virtual TypePtr SemaAnalyze(front::Analyzer &ana) = 0;
  // evaluate AST (if possible)
  virtual std::optional<EvalNum> Eval(front::Evaluator &eval) = 0;
  // generate IR by current AST
  virtual mid::SSAPtr GenerateIR(mid::IRBuilder &irb) = 0;

  // setters
  void set_logger(const front::Logger &logger) { logger_ = logger; }
  void set_module_id(std::string module_id) {
    module_id_ = std::move(module_id);
  }
  void set_abi_reachable(bool value) { abi_reachable_ = value; }
  const TypePtr &set_ast_type(const TypePtr &ast_type) {
    return ast_type_ = ast_type;
  }

  // getters
  [[nodiscard]] const front::Logger &logger() const { return logger_; }
  [[nodiscard]] const TypePtr &ast_type() const { return ast_type_; }
  [[nodiscard]] const std::string &module_id() const { return module_id_; }
  [[nodiscard]] bool abi_reachable() const { return abi_reachable_; }

 private:
  front::Logger logger_;
  TypePtr ast_type_;
  std::string module_id_;
  bool abi_reachable_{};
};

using ASTPtr = std::unique_ptr<BaseAST>;
using ASTPtrList = std::vector<ASTPtr>;

// A source module is parsed once. Imports only add bindings to its definitions.
struct SourceModule {
  std::string id;
  std::string path;
  bool anonymous{};
  ASTPtrList defs;
};

using SourceModulePtr = std::shared_ptr<SourceModule>;

enum class ImportKind : std::uint8_t { Namespace, Selected, Wildcard };

struct ImportBinding {
  std::string name;
  std::string alias;
  // An actual child module with the same path as this selected definition.
  // Sema rejects the import if both interpretations are valid.
  SourceModulePtr module_alternative;
};

// variable/constant definition
class VarLetDefAST : public BaseAST {
 public:
  VarLetDefAST(Property prop, ASTPtrList defs)
      : prop_(prop), defs_(std::move(defs)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] Property prop() const { return prop_; }
  [[nodiscard]] const ASTPtrList &defs() const { return defs_; }

 private:
  Property prop_;
  ASTPtrList defs_;
};

// function definition
class FunDefAST : public BaseAST {
 public:
  FunDefAST(Property prop, std::string id, ASTPtrList args, ASTPtr type,
            ASTPtr body)
      : prop_(prop),
        id_(std::move(id)),
        type_(std::move(type)),
        body_(std::move(body)),
        args_(std::move(args)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] Property prop() const { return prop_; }
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtrList &args() const { return args_; }
  [[nodiscard]] const ASTPtr &type() const { return type_; }
  [[nodiscard]] const ASTPtr &body() const { return body_; }

  // setters
  void set_id(const std::string &id) { id_ = id; }

 private:
  Property prop_;
  std::string id_;
  ASTPtr type_, body_;
  ASTPtrList args_;
};

// declaration
class DeclareAST : public BaseAST {
 public:
  DeclareAST(Property prop, bool is_var, std::string id, ASTPtr type)
      : prop_(prop),
        is_var_(is_var),
        id_(std::move(id)),
        type_(std::move(type)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] Property prop() const { return prop_; }
  [[nodiscard]] bool is_var() const { return is_var_; }
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtr &type() const { return type_; }

  // setters
  void set_id(const std::string &id) { id_ = id; }

 private:
  Property prop_;
  bool is_var_;
  std::string id_;
  ASTPtr type_;
};

// type alias
class TypeAliasAST : public BaseAST {
 public:
  TypeAliasAST(Property prop, std::string id, ASTPtr type)
      : prop_(prop), id_(std::move(id)), type_(std::move(type)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] Property prop() const { return prop_; }
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtr &type() const { return type_; }

 private:
  Property prop_;
  std::string id_;
  ASTPtr type_;
};

// structure definition
class StructAST : public BaseAST {
 public:
  StructAST(Property prop, std::string id, ASTPtrList defs)
      : prop_(prop), id_(std::move(id)), defs_(std::move(defs)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] Property prop() const { return prop_; }
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtrList &defs() const { return defs_; }

 private:
  Property prop_;
  std::string id_;
  ASTPtrList defs_;
};

// enumeration definition
class EnumAST : public BaseAST {
 public:
  EnumAST(Property prop, std::string id, ASTPtr type, ASTPtrList defs)
      : prop_(prop),
        id_(std::move(id)),
        type_(std::move(type)),
        defs_(std::move(defs)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] Property prop() const { return prop_; }
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtr &type() const { return type_; }
  [[nodiscard]] const ASTPtrList &defs() const { return defs_; }

 private:
  Property prop_;
  std::string id_;
  ASTPtr type_;
  ASTPtrList defs_;
};

// imported definitions
class ImportAST : public BaseAST {
 public:
  ImportAST(SourceModulePtr module, ImportKind kind,
            std::vector<ImportBinding> bindings,
            SourceModulePtr alternative_module = {},
            std::string alternative_member = {})
      : module_(std::move(module)),
        kind_(kind),
        bindings_(std::move(bindings)),
        alternative_module_(std::move(alternative_module)),
        alternative_member_(std::move(alternative_member)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtrList &defs() const { return module_->defs; }
  [[nodiscard]] const SourceModulePtr &module() const { return module_; }
  [[nodiscard]] ImportKind kind() const { return kind_; }
  [[nodiscard]] const std::vector<ImportBinding> &bindings() const {
    return bindings_;
  }
  [[nodiscard]] const SourceModulePtr &alternative_module() const {
    return alternative_module_;
  }
  [[nodiscard]] const std::string &alternative_member() const {
    return alternative_member_;
  }

 private:
  SourceModulePtr module_;
  ImportKind kind_;
  std::vector<ImportBinding> bindings_;
  SourceModulePtr alternative_module_;
  std::string alternative_member_;
};

// variable/constant definition element
class VarLetElemAST : public BaseAST {
 public:
  VarLetElemAST(std::string id, ASTPtr type, ASTPtr init, bool is_var)
      : id_(std::move(id)),
        type_(std::move(type)),
        init_(std::move(init)),
        is_var_(is_var) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtr &type() const { return type_; }
  [[nodiscard]] const ASTPtr &init() const { return init_; }
  [[nodiscard]] bool is_var() const { return is_var_; }

  // setters
  void set_id(std::string id) { id_ = std::move(id); }
  void set_init(ASTPtr init) { init_ = std::move(init); }

 private:
  std::string id_;
  ASTPtr type_, init_;
  bool is_var_;
};

// argument definition
class ArgElemAST : public BaseAST {
 public:
  ArgElemAST(std::string id, ASTPtr type)
      : id_(std::move(id)), type_(std::move(type)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtr &type() const { return type_; }

 private:
  std::string id_;
  ASTPtr type_;
};

// element of structure elements
class StructElemAST : public BaseAST {
 public:
  StructElemAST(std::string id, ASTPtr type, bool is_public = false)
      : id_(std::move(id)), type_(std::move(type)), is_public_(is_public) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtr &type() const { return type_; }
  [[nodiscard]] bool is_public() const { return is_public_; }

 private:
  std::string id_;
  ASTPtr type_;
  bool is_public_{};
};

// element of enumeration list
class EnumElemAST : public BaseAST {
 public:
  EnumElemAST(std::string id, ASTPtr expr)
      : id_(std::move(id)), expr_(std::move(expr)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }

  // setters
  void set_expr(ASTPtr expr) { expr_ = std::move(expr); }

 private:
  std::string id_;
  ASTPtr expr_;
};

// statement block
class BlockAST : public BaseAST {
 public:
  explicit BlockAST(ASTPtrList stmts) : stmts_(std::move(stmts)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtrList &stmts() const { return stmts_; }

  // setters
  void set_stmts(ASTPtrList stmts) { stmts_ = std::move(stmts); }
  void set_stmt(std::size_t index, ASTPtr stmt) {
    stmts_[index] = std::move(stmt);
  }

 private:
  ASTPtrList stmts_;
};

// if-else statement
class IfAST : public BaseAST {
 public:
  IfAST(ASTPtr cond, ASTPtr then, ASTPtr else_then)
      : cond_(std::move(cond)),
        then_(std::move(then)),
        else_then_(std::move(else_then)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &cond() const { return cond_; }
  [[nodiscard]] const ASTPtr &then() const { return then_; }
  [[nodiscard]] const ASTPtr &else_then() const { return else_then_; }

  // setters
  void set_cond(ASTPtr cond) { cond_ = std::move(cond); }
  void set_then(ASTPtr then) { then_ = std::move(then); }
  void set_else_then(ASTPtr else_then) { else_then_ = std::move(else_then); }

 private:
  ASTPtr cond_, then_, else_then_;
};

// when-else statement
class WhenAST : public BaseAST {
 public:
  WhenAST(ASTPtr expr, ASTPtrList elems, ASTPtr else_then)
      : expr_(std::move(expr)),
        else_then_(std::move(else_then)),
        elems_(std::move(elems)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }
  [[nodiscard]] const ASTPtrList &elems() const { return elems_; }
  [[nodiscard]] const ASTPtr &else_then() const { return else_then_; }

  // setters
  void set_expr(ASTPtr expr) { expr_ = std::move(expr); }
  void set_else_then(ASTPtr else_then) { else_then_ = std::move(else_then); }

 private:
  ASTPtr expr_, else_then_;
  ASTPtrList elems_;
};

// while statement
class WhileAST : public BaseAST {
 public:
  WhileAST(ASTPtr cond, ASTPtr body)
      : cond_(std::move(cond)), body_(std::move(body)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &cond() const { return cond_; }
  [[nodiscard]] const ASTPtr &body() const { return body_; }

  // setters
  void set_cond(ASTPtr cond) { cond_ = std::move(cond); }
  void set_body(ASTPtr body) { body_ = std::move(body); }

 private:
  ASTPtr cond_, body_;
};

// for-in statement
class ForInAST : public BaseAST {
 public:
  ForInAST(std::string id, ASTPtr expr, ASTPtr body)
      : id_(std::move(id)), expr_(std::move(expr)), body_(std::move(body)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const std::string &next_id() const { return next_id_; }
  [[nodiscard]] const std::string &last_id() const { return last_id_; }
  [[nodiscard]] const TypePtr &id_type() const { return id_type_; }
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }
  [[nodiscard]] const ASTPtr &body() const { return body_; }

  // setters
  void set_next_id(const std::string &next_id) { next_id_ = next_id; }
  void set_last_id(const std::string &last_id) { last_id_ = last_id; }
  void set_id_type(const TypePtr &id_type) { id_type_ = id_type; }
  void set_expr(ASTPtr expr) { expr_ = std::move(expr); }
  void set_body(ASTPtr body) { body_ = std::move(body); }

 private:
  std::string id_, next_id_, last_id_;
  TypePtr id_type_;
  ASTPtr expr_, body_;
};

// inline assembly
class AsmAST : public BaseAST {
 public:
  explicit AsmAST(std::string asm_str) : asm_str_(std::move(asm_str)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &asm_str() const { return asm_str_; }

 private:
  std::string asm_str_;
};

// control statement
class ControlAST : public BaseAST {
 public:
  ControlAST(Keyword type, ASTPtr expr) : type_(type), expr_(std::move(expr)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] Keyword type() const { return type_; }
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }

  // setters
  void set_expr(ASTPtr expr) { expr_ = std::move(expr); }

 private:
  Keyword type_;
  ASTPtr expr_;
};

// when-else element
class WhenElemAST : public BaseAST {
 public:
  WhenElemAST(ASTPtrList conds, ASTPtr body)
      : conds_(std::move(conds)), body_(std::move(body)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtrList &conds() const { return conds_; }
  [[nodiscard]] const ASTPtr &body() const { return body_; }

  // setters
  void set_conds(ASTPtrList conds) { conds_ = std::move(conds); }
  void set_cond(std::size_t index, ASTPtr cond) {
    conds_[index] = std::move(cond);
  }
  void set_body(ASTPtr body) { body_ = std::move(body); }

 private:
  ASTPtrList conds_;
  ASTPtr body_;
};

// binary expression
class BinaryAST : public BaseAST {
 public:
  BinaryAST(Operator op, ASTPtr lhs, ASTPtr rhs)
      : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] Operator op() const { return op_; }
  [[nodiscard]] const std::optional<std::string> &op_func_id() const {
    return op_func_id_;
  }
  [[nodiscard]] const ASTPtr &lhs() const { return lhs_; }
  [[nodiscard]] const ASTPtr &rhs() const { return rhs_; }

  // setters
  void set_op_func_id(const std::string &id) { op_func_id_ = id; }
  void set_lhs(ASTPtr lhs) { lhs_ = std::move(lhs); }
  void set_rhs(ASTPtr rhs) { rhs_ = std::move(rhs); }

 private:
  Operator op_;
  std::optional<std::string> op_func_id_;
  ASTPtr lhs_, rhs_;
};

class AccessAST : public BaseAST {
 public:
  AccessAST(ASTPtr expr, std::string id)
      : id_(std::move(id)), expr_(std::move(expr)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &id() const { return id_; }
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }
  [[nodiscard]] const std::string &resolved_id() const { return resolved_id_; }

  void set_resolved_id(std::string id) { resolved_id_ = std::move(id); }
  ASTPtr TakeExpr() { return std::move(expr_); }

 private:
  std::string id_;
  ASTPtr expr_;
  std::string resolved_id_;
};

// type casting expression
class CastAST : public BaseAST {
 public:
  CastAST(ASTPtr expr, ASTPtr type)
      : expr_(std::move(expr)), type_(std::move(type)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return expr_->IsLiteral(); }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }
  [[nodiscard]] const ASTPtr &type() const { return type_; }

  // setters
  void set_expr(ASTPtr expr) { expr_ = std::move(expr); }

 private:
  ASTPtr expr_, type_;
};

// unary expression
class UnaryAST : public BaseAST {
 public:
  enum class UnaryOp : std::uint8_t {
    Pos,
    Neg,
    LogicNot,
    Not,
    DeRef,
    AddrOf,
    SizeOf,
  };

  UnaryAST(UnaryOp op, ASTPtr opr) : op_(op), opr_(std::move(opr)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] UnaryOp op() const { return op_; }
  [[nodiscard]] const std::optional<std::string> &op_func_id() const {
    return op_func_id_;
  }
  [[nodiscard]] const ASTPtr &opr() const { return opr_; }

  // setters
  void set_op_func_id(const std::string &id) { op_func_id_ = id; }
  void set_opr(ASTPtr opr) { opr_ = std::move(opr); }

 private:
  UnaryOp op_;
  std::optional<std::string> op_func_id_;
  ASTPtr opr_;
};

// indexing
class IndexAST : public BaseAST {
 public:
  IndexAST(ASTPtr expr, ASTPtr index)
      : expr_(std::move(expr)), index_(std::move(index)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }
  [[nodiscard]] const ASTPtr &index() const { return index_; }

  // setter
  void set_index(ASTPtr index) { index_ = std::move(index); }

 private:
  ASTPtr expr_, index_;
};

// function call
class FunCallAST : public BaseAST {
 public:
  enum class CallKind : std::uint8_t { Normal, Dot, Infix };

  FunCallAST(ASTPtr expr, ASTPtrList args,
             CallKind call_kind = CallKind::Normal)
      : expr_(std::move(expr)), args_(std::move(args)), call_kind_(call_kind) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }
  [[nodiscard]] const ASTPtrList &args() const { return args_; }
  [[nodiscard]] CallKind call_kind() const { return call_kind_; }

  // setters
  void set_args(ASTPtrList args) { args_ = std::move(args); }
  void set_arg(std::size_t index, ASTPtr arg) { args_[index] = std::move(arg); }
  void set_expr(ASTPtr expr) { expr_ = std::move(expr); }
  void set_call_kind(CallKind kind) { call_kind_ = kind; }
  ASTPtrList TakeArgs() { return std::move(args_); }

 private:
  ASTPtr expr_;
  ASTPtrList args_;
  CallKind call_kind_;
};

// integer number literal
class IntAST : public BaseAST {
 public:
  explicit IntAST(std::uint64_t value) : value_(value) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return true; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] std::uint64_t value() const { return value_; }

 private:
  std::uint64_t value_;
};

// floating point number literal
class FloatAST : public BaseAST {
 public:
  explicit FloatAST(double value) : value_(value) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return true; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] double value() const { return value_; }

 private:
  double value_;
};

// character literal
class CharAST : public BaseAST {
 public:
  explicit CharAST(std::uint8_t c) : c_(c) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return true; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] std::uint8_t c() const { return c_; }

 private:
  std::uint8_t c_;
};

// identifier
class IdAST : public BaseAST {
 public:
  explicit IdAST(std::string id) : id_(std::move(id)) {}

  [[nodiscard]] bool IsId() const override { return true; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &id() const { return id_; }

  // setters
  void set_id(const std::string &id) { id_ = id; }

 private:
  std::string id_;
};

// string literal
class StringAST : public BaseAST {
 public:
  explicit StringAST(std::string str) : str_(std::move(str)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return true; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &str() const { return str_; }

 private:
  std::string str_;
};

// boolean literal
class BoolAST : public BaseAST {
 public:
  explicit BoolAST(bool value) : value_(value) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return true; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] bool value() const { return value_; }

 private:
  bool value_;
};

// null pointer literal
class NullAST : public BaseAST {
 public:
  NullAST() = default;

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return true; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;
};

// value initializer
class ValInitAST : public BaseAST {
 public:
  ValInitAST(ASTPtr type, ASTPtrList elems)
      : type_(std::move(type)), elems_(std::move(elems)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override {
    for (const auto &i : elems_) {
      if (!i->IsLiteral()) return false;
    }
    return true;
  }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &type() const { return type_; }
  [[nodiscard]] const ASTPtrList &elems() const { return elems_; }

  // setters
  void set_elems(ASTPtrList elems) { elems_ = std::move(elems); }
  void set_elem(std::size_t index, ASTPtr elem) {
    elems_[index] = std::move(elem);
  }

 private:
  ASTPtr type_;
  ASTPtrList elems_;
};

// primitive type
class PrimTypeAST : public BaseAST {
 public:
  explicit PrimTypeAST(Keyword type) : type_(type) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getter
  [[nodiscard]] Keyword type() const { return type_; }

 private:
  Keyword type_;
};

// user defined type
class UserTypeAST : public BaseAST {
 public:
  explicit UserTypeAST(std::string id) : id_(std::move(id)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const std::string &id() const { return id_; }

 private:
  std::string id_;
};

// function types
class FuncTypeAST : public BaseAST {
 public:
  FuncTypeAST(ASTPtrList args, ASTPtr ret)
      : args_(std::move(args)), ret_(std::move(ret)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtrList &args() const { return args_; }
  [[nodiscard]] const ASTPtr &ret() const { return ret_; }

 private:
  ASTPtrList args_;
  ASTPtr ret_;
};

// volatiled type
class VolaTypeAST : public BaseAST {
 public:
  explicit VolaTypeAST(ASTPtr type) : type_(std::move(type)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &type() const { return type_; }

 private:
  ASTPtr type_;
};

// array type
class ArrayTypeAST : public BaseAST {
 public:
  ArrayTypeAST(ASTPtr base, ASTPtr expr)
      : base_(std::move(base)), expr_(std::move(expr)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] const ASTPtr &base() const { return base_; }
  [[nodiscard]] const ASTPtr &expr() const { return expr_; }

  // setter
  void set_expr(ASTPtr expr) { expr_ = std::move(expr); }

 private:
  ASTPtr base_, expr_;
};

// pointer type
class PointerTypeAST : public BaseAST {
 public:
  PointerTypeAST(bool is_var, ASTPtr base)
      : is_var_(is_var), base_(std::move(base)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] bool is_var() const { return is_var_; }
  [[nodiscard]] const ASTPtr &base() const { return base_; }

 private:
  bool is_var_;
  ASTPtr base_;
};

// reference type
class RefTypeAST : public BaseAST {
 public:
  RefTypeAST(bool is_var, ASTPtr base)
      : is_var_(is_var), base_(std::move(base)) {}

  [[nodiscard]] bool IsId() const override { return false; }
  [[nodiscard]] bool IsLiteral() const override { return false; }

  void Dump(std::ostream &os) const override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Evaluator &eval) override;
  mid::SSAPtr GenerateIR(mid::IRBuilder &irb) override;

  // getters
  [[nodiscard]] bool is_var() const { return is_var_; }
  [[nodiscard]] const ASTPtr &base() const { return base_; }

 private:
  bool is_var_;
  ASTPtr base_;
};

}  // namespace yulang::define

#endif  // YULANG_DEFINE_AST_H_
