#include "define/ast.h"

#include "back/irbuilder.h"

using namespace yulang::define;
using namespace yulang::back;

IRPtr VarLetDefAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr FunDefAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr DeclareAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr TypeAliasAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr StructAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr EnumAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr ImportAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr VarLetElemAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr ArgElemAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr StructElemAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr EnumElemAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr BlockAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr IfAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr WhenAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr WhileAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr ForInAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr AsmAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr ControlAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr WhenElemAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr BinaryAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr AccessAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr CastAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr UnaryAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr IndexAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr FunCallAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr IntAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr FloatAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr CharAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr IdAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr StringAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr BoolAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr NullAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr ValInitAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr PrimTypeAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr UserTypeAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr FuncTypeAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr VolaTypeAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr ArrayTypeAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr PointerTypeAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}

IRPtr RefTypeAST::GenerateIR(IRBuilder &builder) {
  return builder.GenerateOn(*this);
}
