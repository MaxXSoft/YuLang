#include "define/ast.h"

#include "back/codegen.h"

using namespace yulang::define;
using namespace yulang::back;

CodePtr VarLetDefAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr FunDefAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr DeclareAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr TypeAliasAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr StructAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr EnumAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr ImportAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr VarLetElemAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr ArgElemAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr StructElemAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr EnumElemAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr BlockAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr IfAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr WhenAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr WhileAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr ForInAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr AsmAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr ControlAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr WhenElemAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr BinaryAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr AccessAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr CastAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr UnaryAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr IndexAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr FunCallAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr IntAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr FloatAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr CharAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr IdAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr StringAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr BoolAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr NullAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr ValInitAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr PrimTypeAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr UserTypeAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr FuncTypeAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr VolaTypeAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr ArrayTypeAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr PointerTypeAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}

CodePtr RefTypeAST::GenerateCode(CodeGen &gen) {
  return gen.GenerateOn(*this);
}
