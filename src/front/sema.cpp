#include "define/ast.h"

#include "front/analyzer.h"
#include "front/eval.h"

using namespace yulang::define;
using namespace yulang::front;

TypePtr VarLetDefAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr FunDefAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr DeclareAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr TypeAliasAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr StructAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr EnumAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr ImportAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr VarLetElemAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr ArgElemAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr StructElemAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr EnumElemAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr BlockAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr IfAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr WhenAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr WhileAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr ForInAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr AsmAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr ControlAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr WhenElemAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr BinaryAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr AccessAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr CastAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr UnaryAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr IndexAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr FunCallAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr IntAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr FloatAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr CharAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr IdAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr StringAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr BoolAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr NullAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr ValInitAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr PrimTypeAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr UserTypeAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr FuncTypeAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr VolaTypeAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr ArrayTypeAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr PointerTypeAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr RefTypeAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

std::optional<EvalNum> VarLetDefAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> FunDefAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> DeclareAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> TypeAliasAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> StructAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> EnumAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> ImportAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> VarLetElemAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> ArgElemAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> StructElemAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> EnumElemAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> BlockAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> IfAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> WhenAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> WhileAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> ForInAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> AsmAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> ControlAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> WhenElemAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> BinaryAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> AccessAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> CastAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> UnaryAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> IndexAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> FunCallAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> IntAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> FloatAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> CharAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> IdAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> StringAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> BoolAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> NullAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> ValInitAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> PrimTypeAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> UserTypeAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> FuncTypeAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> VolaTypeAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> ArrayTypeAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> PointerTypeAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}

std::optional<EvalNum> RefTypeAST::Eval(Evaluator &eval) {
  return eval.EvalOn(*this);
}
