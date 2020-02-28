#include "define/ast.h"

#include "front/analyzer.h"

using namespace yulang::define;
using namespace yulang::front;

TypePtr PropertyAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

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

TypePtr VarElemAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr LetElemAST::SemaAnalyze(Analyzer &ana) {
  return ana.AnalyzeOn(*this);
}

TypePtr ArgElemAST::SemaAnalyze(Analyzer &ana) {
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

std::optional<EvalNum> PropertyAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> VarLetDefAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> FunDefAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> DeclareAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> TypeAliasAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> StructAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> EnumAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> ImportAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> VarElemAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> LetElemAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> ArgElemAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> EnumElemAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> BlockAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> IfAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> WhenAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> WhileAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> ForInAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> AsmAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> ControlAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> WhenElemAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> BinaryAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> CastAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> UnaryAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> IndexAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> FunCallAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> IntAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> FloatAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> CharAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> IdAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> StringAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> BoolAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> NullAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> ValInitAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> PrimTypeAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> UserTypeAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> FuncTypeAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> VolaTypeAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> ArrayTypeAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> PointerTypeAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
std::optional<EvalNum> RefTypeAST::Eval(Analyzer &ana) {
  return ana.EvalOn(*this);
}
