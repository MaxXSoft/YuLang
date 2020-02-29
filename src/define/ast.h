#ifndef YULANG_DEFINE_AST_H_
#define YULANG_DEFINE_AST_H_

#include <memory>
#include <vector>
#include <utility>
#include <string>
#include <optional>
#include <cstdint>

#include "front/logger.h"
#include "define/token.h"
#include "define/type.h"
#include "define/symbol.h"

// forward declarations
namespace yulang::front {
class Analyzer;
}  // namespace yulang::front

namespace yulang::define {

// definition of base class of all ASTs
class BaseAST {
 public:
  virtual ~BaseAST() = default;

  // dump the content of AST to output stream
  virtual void Dump(std::ostream &os) = 0;
  // run sematic analysis on current AST
  virtual TypePtr SemaAnalyze(front::Analyzer &ana) = 0;
  // evaluate AST (if possible)
  virtual std::optional<EvalNum> Eval(front::Analyzer &ana) = 0;

  // setters
  void set_logger(const front::Logger &logger) { logger_ = logger; }
  const TypePtr &set_ast_type(const TypePtr &ast_type) {
    return ast_type_ = ast_type;
  }

  // getters
  const front::Logger &logger() const { return logger_; }
  const TypePtr &ast_type() const { return ast_type_; }

 private:
  front::Logger logger_;
  TypePtr ast_type_;
};

using ASTPtr = std::unique_ptr<BaseAST>;
using ASTPtrList = std::vector<ASTPtr>;

// property (public/extern)
class PropertyAST : public BaseAST {
 public:
  enum class Property { None, Public, Extern, Demangle };

  PropertyAST(Property prop) : prop_(prop) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  Property prop() const { return prop_; }

 private:
  Property prop_;
};

// variable/constant definition
class VarLetDefAST : public BaseAST {
 public:
  VarLetDefAST(ASTPtr prop, ASTPtrList defs)
      : prop_(std::move(prop)), defs_(std::move(defs)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &prop() const { return prop_; }
  const ASTPtrList &defs() const { return defs_; }

 private:
  ASTPtr prop_;
  ASTPtrList defs_;
};

// function definition
class FunDefAST : public BaseAST {
 public:
  FunDefAST(ASTPtr prop, const std::string &id, ASTPtrList args,
            ASTPtr type, ASTPtr body)
      : id_(id), prop_(std::move(prop)), type_(std::move(type)),
        body_(std::move(body)), args_(std::move(args)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &prop() const { return prop_; }
  const std::string &id() const { return id_; }
  const ASTPtrList &args() const { return args_; }
  const ASTPtr &type() const { return type_; }
  const ASTPtr &body() const { return body_; }

 private:
  std::string id_;
  ASTPtr prop_, type_, body_;
  ASTPtrList args_;
};

// declaration
class DeclareAST : public BaseAST {
 public:
  DeclareAST(ASTPtr prop, const std::string &id, ASTPtr type)
      : id_(id), prop_(std::move(prop)), type_(std::move(type)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &prop() const { return prop_; }
  const std::string &id() const { return id_; }
  const ASTPtr &type() const { return type_; }

 private:
  std::string id_;
  ASTPtr prop_, type_;
};

// type alias
class TypeAliasAST : public BaseAST {
 public:
  TypeAliasAST(ASTPtr prop, const std::string &id, ASTPtr type)
      : id_(id), prop_(std::move(prop)), type_(std::move(type)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &prop() const { return prop_; }
  const std::string &id() const { return id_; }
  const ASTPtr &type() const { return type_; }

 private:
  std::string id_;
  ASTPtr prop_, type_;
};

// structure definition
class StructAST : public BaseAST {
 public:
  StructAST(ASTPtr prop, const std::string &id, ASTPtrList defs)
      : id_(id), prop_(std::move(prop)), defs_(std::move(defs)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &prop() const { return prop_; }
  const std::string &id() const { return id_; }
  const ASTPtrList &defs() const { return defs_; }

 private:
  std::string id_;
  ASTPtr prop_;
  ASTPtrList defs_;
};

// enumeration definition
class EnumAST : public BaseAST {
 public:
  EnumAST(ASTPtr prop, const std::string &id, ASTPtr type, ASTPtrList defs)
      : id_(id), prop_(std::move(prop)), type_(std::move(type)),
        defs_(std::move(defs)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &prop() const { return prop_; }
  const std::string &id() const { return id_; }
  const ASTPtr &type() const { return type_; }
  const ASTPtrList &defs() const { return defs_; }

 private:
  std::string id_;
  ASTPtr prop_, type_;
  ASTPtrList defs_;
};

// imported definitions
class ImportAST : public BaseAST {
 public:
  ImportAST(ASTPtrList defs) : defs_(std::move(defs)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtrList &defs() const { return defs_; }

 private:
  ASTPtrList defs_;
};

// variable definition element
class VarElemAST : public BaseAST {
 public:
  VarElemAST(const std::string &id, ASTPtr type, ASTPtr init)
      : id_(id), type_(std::move(type)), init_(std::move(init)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &id() const { return id_; }
  const ASTPtr &type() const { return type_; }
  const ASTPtr &init() const { return init_; }

 private:
  std::string id_;
  ASTPtr type_, init_;
};

// constant definition element
class LetElemAST : public BaseAST {
 public:
  LetElemAST(const std::string &id, ASTPtr type, ASTPtr init)
      : id_(id), type_(std::move(type)), init_(std::move(init)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &id() const { return id_; }
  const ASTPtr &type() const { return type_; }
  const ASTPtr &init() const { return init_; }

 private:
  std::string id_;
  ASTPtr type_, init_;
};

// argument definition
class ArgElemAST : public BaseAST {
 public:
  ArgElemAST(const std::string &id, ASTPtr type)
      : id_(id), type_(std::move(type)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &id() const { return id_; }
  const ASTPtr &type() const { return type_; }

 private:
  std::string id_;
  ASTPtr type_;
};

// element of enumeration list
class EnumElemAST : public BaseAST {
 public:
  EnumElemAST(const std::string &id, ASTPtr expr)
      : id_(id), expr_(std::move(expr)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &id() const { return id_; }
  const ASTPtr &expr() const { return expr_; }

 private:
  std::string id_;
  ASTPtr expr_;
};

// statement block
class BlockAST : public BaseAST {
 public:
  BlockAST(ASTPtrList stmts) : stmts_(std::move(stmts)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtrList &stmts() const { return stmts_; }

 private:
  ASTPtrList stmts_;
};

// if-else statement
class IfAST : public BaseAST {
 public:
  IfAST(ASTPtr cond, ASTPtr then, ASTPtr else_then)
      : cond_(std::move(cond)), then_(std::move(then)),
        else_then_(std::move(else_then)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &cond() const { return cond_; }
  const ASTPtr &then() const { return then_; }
  const ASTPtr &else_then() const { return else_then_; }

 private:
  ASTPtr cond_, then_, else_then_;
};

// when-else statement
class WhenAST : public BaseAST {
 public:
  WhenAST(ASTPtr expr, ASTPtrList elems, ASTPtr else_then)
      : expr_(std::move(expr)), else_then_(std::move(else_then)),
        elems_(std::move(elems)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &expr() const { return expr_; }
  const ASTPtrList &elems() const { return elems_; }
  const ASTPtr &else_then() const { return else_then_; }

 private:
  ASTPtr expr_, else_then_;
  ASTPtrList elems_;
};

// while statement
class WhileAST : public BaseAST {
 public:
  WhileAST(ASTPtr cond, ASTPtr body)
      : cond_(std::move(cond)), body_(std::move(body)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &cond() const { return cond_; }
  const ASTPtr &body() const { return body_; }

 private:
  ASTPtr cond_, body_;
};

// for-in statement
class ForInAST : public BaseAST {
 public:
  ForInAST(const std::string &id, ASTPtr expr, ASTPtr body)
      : id_(id), expr_(std::move(expr)), body_(std::move(body)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &id() const { return id_; }
  const ASTPtr &expr() const { return expr_; }
  const ASTPtr &body() const { return body_; }

 private:
  std::string id_;
  ASTPtr expr_, body_;
};

// inline assembly
class AsmAST : public BaseAST {
 public:
  AsmAST(const std::string &asm_str) : asm_str_(asm_str) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &asm_str() const { return asm_str_; }

 private:
  std::string asm_str_;
};

// control statement
class ControlAST : public BaseAST {
 public:
  ControlAST(Keyword type, ASTPtr expr)
      : type_(type), expr_(std::move(expr)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  Keyword type() const { return type_; }
  const ASTPtr &expr() const { return expr_; }

 private:
  Keyword type_;
  ASTPtr expr_;
};

// when-else element
class WhenElemAST : public BaseAST {
 public:
  WhenElemAST(ASTPtrList conds, ASTPtr body)
      : conds_(std::move(conds)), body_(std::move(body)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtrList &conds() const { return conds_; }
  const ASTPtr &body() const { return body_; }

 private:
  ASTPtrList conds_;
  ASTPtr body_;
};

// binary expression
class BinaryAST : public BaseAST {
 public:
  BinaryAST(Operator op, ASTPtr lhs, ASTPtr rhs)
      : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  Operator op() const { return op_; }
  const ASTPtr &lhs() const { return lhs_; }
  const ASTPtr &rhs() const { return rhs_; }

 private:
  Operator op_;
  ASTPtr lhs_, rhs_;
};

// type casting expression
class CastAST : public BaseAST {
 public:
  CastAST(ASTPtr expr, ASTPtr type)
      : expr_(std::move(expr)), type_(std::move(type)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &expr() const { return expr_; }
  const ASTPtr &type() const { return type_; }

 private:
  ASTPtr expr_, type_;
};

// unary expression
class UnaryAST : public BaseAST {
 public:
  enum class UnaryOp { Pos, Neg, LogicNot, Not, DeRef, AddrOf, SizeOf };

  UnaryAST(UnaryOp op, ASTPtr opr)
      : op_(op), opr_(std::move(opr)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  UnaryOp op() const { return op_; }
  const ASTPtr &opr() const { return opr_; }

 private:
  UnaryOp op_;
  ASTPtr opr_;
};

// indexing
class IndexAST : public BaseAST {
 public:
  IndexAST(ASTPtr expr, ASTPtr index)
      : expr_(std::move(expr)), index_(std::move(index)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &expr() const { return expr_; }
  const ASTPtr &index() const { return index_; }

 private:
  ASTPtr expr_, index_;
};

// function call
class FunCallAST : public BaseAST {
 public:
  FunCallAST(ASTPtr expr, ASTPtrList args)
      : expr_(std::move(expr)), args_(std::move(args)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &expr() const { return expr_; }
  const ASTPtrList &args() const { return args_; }

 private:
  ASTPtr expr_;
  ASTPtrList args_;
};

// integer number literal
class IntAST : public BaseAST {
 public:
  IntAST(std::uint64_t value) : value_(value) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  std::uint64_t value() const { return value_; }

 private:
  std::uint64_t value_;
};

// floating point number literal
class FloatAST : public BaseAST {
 public:
  FloatAST(double value) : value_(value) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  double value() const { return value_; }

 private:
  double value_;
};

// character literal
class CharAST : public BaseAST {
 public:
  CharAST(std::uint8_t c) : c_(c) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  std::uint8_t c() const { return c_; }

 private:
  std::uint8_t c_;
};

// identifier
class IdAST : public BaseAST {
 public:
  IdAST(const std::string &id) : id_(id) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &id() const { return id_; }

 private:
  std::string id_;
};

// string literal
class StringAST : public BaseAST {
 public:
  StringAST(const std::string &str) : str_(str) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &str() const { return str_; }

 private:
  std::string str_;
};

// boolean literal
class BoolAST : public BaseAST {
 public:
  BoolAST(bool value) : value_(value) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  bool value() const { return value_; }

 private:
  bool value_;
};

// null pointer literal
class NullAST : public BaseAST {
 public:
  NullAST() {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;
};

// value initializer
class ValInitAST : public BaseAST {
 public:
  ValInitAST(ASTPtr type, ASTPtrList elems)
      : type_(std::move(type)), elems_(std::move(elems)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &type() const { return type_; }
  const ASTPtrList &elems() const { return elems_; }

 private:
  ASTPtr type_;
  ASTPtrList elems_;
};

// primitive type
class PrimTypeAST : public BaseAST {
 public:
  PrimTypeAST(Keyword type) : type_(type) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getter
  Keyword type() const { return type_; }

 private:
  Keyword type_;
};

// user defined type
class UserTypeAST : public BaseAST {
 public:
  UserTypeAST(const std::string &id) : id_(id) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const std::string &id() const { return id_; }

 private:
  std::string id_;
};

// function types
class FuncTypeAST : public BaseAST {
 public:
  FuncTypeAST(ASTPtrList args, ASTPtr ret)
      : args_(std::move(args)), ret_(std::move(ret)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtrList &args() const { return args_; }
  const ASTPtr &ret() const { return ret_; }

 private:
  ASTPtrList args_;
  ASTPtr ret_;
};

// volatiled type
class VolaTypeAST : public BaseAST {
 public:
  VolaTypeAST(ASTPtr type) : type_(std::move(type)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &type() const { return type_; }

 private:
  ASTPtr type_;
};

// array type
class ArrayTypeAST : public BaseAST {
 public:
  ArrayTypeAST(ASTPtr base, ASTPtr expr)
      : base_(std::move(base)), expr_(std::move(expr)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  const ASTPtr &base() const { return base_; }
  const ASTPtr &expr() const { return expr_; }

 private:
  ASTPtr base_, expr_;
};

// pointer type
class PointerTypeAST : public BaseAST {
 public:
  PointerTypeAST(bool is_var, ASTPtr base)
      : is_var_(is_var), base_(std::move(base)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  bool is_var() const { return is_var_; }
  const ASTPtr &base() const { return base_; }

 private:
  bool is_var_;
  ASTPtr base_;
};

// reference type
class RefTypeAST : public BaseAST {
 public:
  RefTypeAST(bool is_var, ASTPtr base)
      : is_var_(is_var), base_(std::move(base)) {}

  void Dump(std::ostream &os) override;
  TypePtr SemaAnalyze(front::Analyzer &ana) override;
  std::optional<EvalNum> Eval(front::Analyzer &ana) override;

  // getters
  bool is_var() const { return is_var_; }
  const ASTPtr &base() const { return base_; }

 private:
  bool is_var_;
  ASTPtr base_;
};

}  // namespace yulang::define

#endif  // YULANG_DEFINE_AST_H_
