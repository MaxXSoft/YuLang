#include "define/ast.h"

#include "mid/irbuilder.h"

using namespace yulang::define;
using namespace yulang::mid;

SSAPtr VarLetDefAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr FunDefAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr DeclareAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr TypeAliasAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr StructAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr EnumAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr ImportAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr VarLetElemAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr ArgElemAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr StructElemAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr EnumElemAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr BlockAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr IfAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr WhenAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr WhileAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr ForInAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr AsmAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr ControlAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr WhenElemAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr BinaryAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr AccessAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr CastAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr UnaryAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr IndexAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr FunCallAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr IntAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr FloatAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr CharAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr IdAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr StringAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr BoolAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr NullAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr ValInitAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr PrimTypeAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr UserTypeAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr FuncTypeAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr VolaTypeAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr ArrayTypeAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr PointerTypeAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}

SSAPtr RefTypeAST::GenerateIR(IRBuilder &irb) {
  return irb.GenerateOn(*this);
}
