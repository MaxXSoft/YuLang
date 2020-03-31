#include "mid/ssa.h"

#include <iomanip>
#include <cctype>

#include "xstl/guard.h"

using namespace yulang::mid;
using namespace yulang::define;

namespace {

// indention
const char *kIndent = "  ";

// linkage types
const char *kLinkTypes[] = {
  "internal", "inline", "external", "global_ctor", "global_dtor",
};

// binary operators
const char *kBinOps[] = {
  "add", "sub", "mul", "udiv", "sdiv", "urem", "srem",
  "eq", "neq", "lt", "le", "gt", "ge",
  "and", "or", "xor", "shl", "lshr", "ashr",
  "fadd", "fsub", "fmul", "fdiv", "frem",
  "feq", "fne", "flt", "fle", "fgt", "fge",
};

// unary operators
const char *kUnaOps[] = {
  "neg", "lnot", "not", "fneg", "cast",
};

// indicate if is in expression
int in_expr = 0;

xstl::Guard InExpr() {
  ++in_expr;
  return xstl::Guard([] { --in_expr; });
}

void ConvertChar(std::ostream &os, char c) {
  switch (c) {
    case '\a':  os << "\\a";  break;
    case '\b':  os << "\\b";  break;
    case '\f':  os << "\\f";  break;
    case '\n':  os << "\\n";  break;
    case '\r':  os << "\\r";  break;
    case '\t':  os << "\\t";  break;
    case '\v':  os << "\\v'"; break;
    case '\\':  os << "\\\\"; break;
    case '\'':  os << "'";    break;
    case '"':   os << "\\\""; break;
    case '\0':  os << "\\0";  break;
    default: {
      if (std::isprint(c)) {
        os << c;
      }
      else {
        os << "\\x" << std::setw(2) << std::setfill('0') << std::hex
           << static_cast<int>(c);
      }
      break;
    }
  }
}

inline void PrintId(std::ostream &os, IdManager &idm, const Value *val) {
  if (auto name = idm.GetName(val)) {
    os << '@' << *name;
  }
  else {
    os << '%' << idm.GetId(val);
  }
}

inline void PrintId(std::ostream &os, IdManager &idm, const SSAPtr &val) {
  PrintId(os, idm, val.get());
}

inline void PrintId(std::ostream &os, IdManager &idm, const Use &use) {
  PrintId(os, idm, use.value().get());
}

inline void PrintType(std::ostream &os, const TypePtr &type) {
  os << type->GetTypeId();
}

inline void PrintType(std::ostream &os, const Value *val) {
  PrintType(os, val->type());
}

inline void PrintType(std::ostream &os, const SSAPtr &val) {
  PrintType(os, val->type());
}

inline void PrintType(std::ostream &os, const Use &use) {
  PrintType(os, use.value()->type());
}

template <typename T>
inline void PrintIdType(std::ostream &os, IdManager &idm, T &&val) {
  PrintType(os, val);
  os << ' ';
  PrintId(os, idm, val);
}

template <typename T>
inline void PrintPrefix(std::ostream &os, IdManager &idm, T &&val) {
  os << kIndent;
  PrintId(os, idm, val);
  os << " = ";
}

template <typename It>
inline void PrintIdList(std::ostream &os, IdManager &idm,
                        It begin, It end) {
  bool need_sep = false;
  for (auto it = begin; it != end; ++it) {
    if (need_sep) {
      os << ", ";
    }
    else {
      need_sep = true;
    }
    PrintId(os, idm, *it);
  }
}

}  // namespace

void LoadSSA::Dump(std::ostream &os, IdManager &idm) const {
  PrintPrefix(os, idm, this);
  os << "load ";
  PrintType(os, type());
  os << ", ";
  PrintIdType(os, idm, (*this)[0]);
  os << std::endl;
}

void StoreSSA::Dump(std::ostream &os, IdManager &idm) const {
  os << kIndent << "store ";
  PrintIdType(os, idm, (*this)[0]);
  os << ", ";
  PrintIdType(os, idm, (*this)[1]);
  os << std::endl;
}

void AccessSSA::Dump(std::ostream &os, IdManager &idm) const {
  PrintPrefix(os, idm, this);
  os << "access ";
  if (acc_type_ == AccessType::Pointer) {
    os << "ptr ";
  }
  else {
    os << "elem ";
  }
  PrintIdType(os, idm, (*this)[0]);
  os << ", ";
  PrintIdType(os, idm, (*this)[1]);
  os << std::endl;
}

void BinarySSA::Dump(std::ostream &os, IdManager &idm) const {
  PrintPrefix(os, idm, this);
  os << kBinOps[static_cast<int>(op_)] << ' ';
  PrintType(os, this);
  os << ' ';
  PrintId(os, idm, (*this)[0]);
  os << ", ";
  PrintId(os, idm, (*this)[1]);
  os << std::endl;
}

void UnarySSA::Dump(std::ostream &os, IdManager &idm) const {
  PrintPrefix(os, idm, this);
  os << kUnaOps[static_cast<int>(op_)] << ' ';
  PrintType(os, this);
  os << ' ';
  PrintId(os, idm, (*this)[0]);
  os << std::endl;
}

void CallSSA::Dump(std::ostream &os, IdManager &idm) const {
  PrintPrefix(os, idm, this);
  os << "call ";
  PrintIdType(os, idm, (*this)[0]);
  for (int i = 1; i < size(); ++i) {
    os << ", ";
    PrintId(os, idm, (*this)[i]);
  }
  os << std::endl;
}

void BranchSSA::Dump(std::ostream &os, IdManager &idm) const {
  os << kIndent << "branch ";
  PrintIdType(os, idm, (*this)[0]);
  os << ", ";
  PrintId(os, idm, (*this)[1]);
  os << ", ";
  PrintId(os, idm, (*this)[2]);
  os << std::endl;
}

void JumpSSA::Dump(std::ostream &os, IdManager &idm) const {
  os << kIndent << "jump ";
  PrintId(os, idm, (*this)[0]);
  os << std::endl;
}

void ReturnSSA::Dump(std::ostream &os, IdManager &idm) const {
  os << kIndent << "return ";
  if (!(*this)[0].value()) {
    os << "void";
  }
  else {
    PrintIdType(os, idm, (*this)[0]);
  }
  os << std::endl;
}

void FunctionSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (size()) {
    os << "define ";
  }
  else {
    os << "declare ";
  }
  os << kLinkTypes[static_cast<int>(link_)] << ' ';
  idm.LogName(this, name_);
  PrintIdType(os, idm, this);
  if (size()) {
    os << " {" << std::endl;
    idm.ResetId();
    for (const auto &i : *this) {
      i.value()->Dump(os, idm);
    }
    os << '}';
  }
  os << std::endl;
}

void GlobalVarSSA::Dump(std::ostream &os, IdManager &idm) const {
  idm.LogName(this, name_);
  PrintId(os, idm, this);
  os << " = " << kLinkTypes[static_cast<int>(link_)] << " global ";
  PrintType(os, this);
  if ((*this)[0].value()) {
    auto inex = InExpr();
    os << ", ";
    (*this)[0].value()->Dump(os, idm);
  }
  os << std::endl;
}

void AllocaSSA::Dump(std::ostream &os, IdManager &idm) const {
  PrintPrefix(os, idm, this);
  os << "alloca ";
  PrintType(os, this);
  os << std::endl;
}

void BlockSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (!name_.empty()) idm.LogName(this, name_);
  PrintId(os, idm, this);
  os << ':';
  if (!preds_.empty()) {
    os << " ; preds: ";
    PrintIdList(os, idm, preds_.begin(), preds_.end());
  }
  if (!succs_.empty() && succs_.front()) {
    os << " ; succs: ";
    PrintIdList(os, idm, succs_.begin(), succs_.end());
  }
  os << std::endl;
  for (const auto &i : insts_) {
    i->Dump(os, idm);
  }
}

void ArgRefSSA::Dump(std::ostream &os, IdManager &idm) const {
  PrintPrefix(os, idm, this);
  os << "arg " << index_ << ", ";
  PrintIdType(os, idm, func_);
  os << std::endl;
}

void AsmSSA::Dump(std::ostream &os, IdManager &idm) const {
  os << kIndent << "asm \"";
  for (const auto &c : asm_str_) ConvertChar(os, c);
  os << '"' << std::endl;
}

void ConstIntSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (!in_expr) PrintPrefix(os, idm, this);
  os << "constant ";
  PrintType(os, this);
  os << ' ' << value_;
  if (!in_expr) os << std::endl;
}

void ConstFloatSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (!in_expr) PrintPrefix(os, idm, this);
  os << "constant ";
  PrintType(os, this);
  os << ' ' << value_;
  if (!in_expr) os << std::endl;
}

void ConstStrSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (!in_expr) PrintPrefix(os, idm, this);
  os << "constant ";
  PrintType(os, this);
  os << " \"";
  for (const auto &c : str_) ConvertChar(os, c);
  os << '"';
  if (!in_expr) os << std::endl;
}

void ConstStructSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (!in_expr) PrintPrefix(os, idm, this);
  os << "constant ";
  PrintType(os, this);
  os << " {";
  {
    auto inex = InExpr();
    for (int i = 0; i < size(); ++i) {
      if (i) os << ", ";
      (*this)[i].value()->Dump(os, idm);
    }
  }
  os << '}';
  if (!in_expr) os << std::endl;
}

void ConstArraySSA::Dump(std::ostream &os, IdManager &idm) const {
  if (!in_expr) PrintPrefix(os, idm, this);
  os << "constant ";
  PrintType(os, this);
  os << " {";
  {
    auto inex = InExpr();
    for (int i = 0; i < size(); ++i) {
      if (i) os << ", ";
      (*this)[i].value()->Dump(os, idm);
    }
  }
  os << '}';
  if (!in_expr) os << std::endl;
}

void ConstZeroSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (!in_expr) PrintPrefix(os, idm, this);
  os << "constant ";
  PrintType(os, this);
  os << " zero";
  if (!in_expr) os << std::endl;
}
