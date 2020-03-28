#include "define/ast.h"

#include "mid/irgen.h"

using namespace yulang::define;
using namespace yulang::mid;

SSAPtr VarLetDefAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr FunDefAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr DeclareAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr TypeAliasAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr StructAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr EnumAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr ImportAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr VarLetElemAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr ArgElemAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr StructElemAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr EnumElemAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr BlockAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr IfAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr WhenAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr WhileAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr ForInAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr AsmAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr ControlAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr WhenElemAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr BinaryAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr AccessAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr CastAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr UnaryAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr IndexAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr FunCallAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr IntAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr FloatAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr CharAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr IdAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr StringAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr BoolAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr NullAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr ValInitAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr PrimTypeAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr UserTypeAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr FuncTypeAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr VolaTypeAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr ArrayTypeAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr PointerTypeAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}

SSAPtr RefTypeAST::GenerateIR(mid::IRGen &gen) {
  return gen.GenerateOn(*this);
}
