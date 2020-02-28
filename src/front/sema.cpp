#include "define/ast.h"

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
