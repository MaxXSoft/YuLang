#ifndef YULANG_FRONT_ANALYZER_H_
#define YULANG_FRONT_ANALYZER_H_

#include "define/type.h"

namespace yulang::front {

class Analyzer {
 public:
  Analyzer() {}

  define::TypePtr AnalyzeOn(define::PropertyAST &ast);
  define::TypePtr AnalyzeOn(define::VarLetDefAST &ast);
  define::TypePtr AnalyzeOn(define::FunDefAST &ast);
  define::TypePtr AnalyzeOn(define::DeclareAST &ast);
  define::TypePtr AnalyzeOn(define::TypeAliasAST &ast);
  define::TypePtr AnalyzeOn(define::StructAST &ast);
  define::TypePtr AnalyzeOn(define::EnumAST &ast);
  define::TypePtr AnalyzeOn(define::ImportAST &ast);
  define::TypePtr AnalyzeOn(define::VarElemAST &ast);
  define::TypePtr AnalyzeOn(define::LetElemAST &ast);
  define::TypePtr AnalyzeOn(define::ArgElemAST &ast);
  define::TypePtr AnalyzeOn(define::EnumElemAST &ast);
  define::TypePtr AnalyzeOn(define::BlockAST &ast);
  define::TypePtr AnalyzeOn(define::IfAST &ast);
  define::TypePtr AnalyzeOn(define::WhenAST &ast);
  define::TypePtr AnalyzeOn(define::WhileAST &ast);
  define::TypePtr AnalyzeOn(define::ForInAST &ast);
  define::TypePtr AnalyzeOn(define::AsmAST &ast);
  define::TypePtr AnalyzeOn(define::ControlAST &ast);
  define::TypePtr AnalyzeOn(define::WhenElemAST &ast);
  define::TypePtr AnalyzeOn(define::BinaryAST &ast);
  define::TypePtr AnalyzeOn(define::CastAST &ast);
  define::TypePtr AnalyzeOn(define::UnaryAST &ast);
  define::TypePtr AnalyzeOn(define::IndexAST &ast);
  define::TypePtr AnalyzeOn(define::FunCallAST &ast);
  define::TypePtr AnalyzeOn(define::IntAST &ast);
  define::TypePtr AnalyzeOn(define::FloatAST &ast);
  define::TypePtr AnalyzeOn(define::CharAST &ast);
  define::TypePtr AnalyzeOn(define::IdAST &ast);
  define::TypePtr AnalyzeOn(define::StringAST &ast);
  define::TypePtr AnalyzeOn(define::BoolAST &ast);
  define::TypePtr AnalyzeOn(define::NullAST &ast);
  define::TypePtr AnalyzeOn(define::ValInitAST &ast);
  define::TypePtr AnalyzeOn(define::PrimTypeAST &ast);
  define::TypePtr AnalyzeOn(define::UserTypeAST &ast);
  define::TypePtr AnalyzeOn(define::FuncTypeAST &ast);
  define::TypePtr AnalyzeOn(define::VolaTypeAST &ast);
  define::TypePtr AnalyzeOn(define::ArrayTypeAST &ast);
  define::TypePtr AnalyzeOn(define::PointerTypeAST &ast);
  define::TypePtr AnalyzeOn(define::RefTypeAST &ast);

 private:
  //
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_ANALYZER_H_
