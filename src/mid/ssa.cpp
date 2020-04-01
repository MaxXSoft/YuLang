#include "mid/ssa.h"

#include <iomanip>
#include <streambuf>
#include <cctype>
#include <cassert>

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

// null stream buffer
class : public std::streambuf {
 public:
  int overflow(int c) override { return c; }
} null_buffer;

// null output stream
std::ostream null_os(&null_buffer);

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

inline void PrintType(std::ostream &os, const TypePtr &type) {
  os << type->GetTypeId();
}

inline void DumpVal(std::ostream &os, IdManager &idm, const SSAPtr &val) {
  val->Dump(os, idm);
}

inline void DumpVal(std::ostream &os, IdManager &idm, const Use &use) {
  DumpVal(os, idm, use.value());
}

template <typename It>
inline void DumpVal(std::ostream &os, IdManager &idm, It begin, It end) {
  bool need_sep = false;
  for (auto it = begin; it != end; ++it) {
    if (need_sep) {
      os << ", ";
    }
    else {
      need_sep = true;
    }
    DumpVal(os, idm, *it);
  }
}

inline void DumpWithType(std::ostream &os, IdManager &idm,
                         const SSAPtr &val) {
  PrintType(os, val->type());
  os << ' ';
  DumpVal(os, idm, val);
}

inline void DumpWithType(std::ostream &os, IdManager &idm,
                         const Use &use) {
  PrintType(os, use.value()->type());
  os << ' ';
  DumpVal(os, idm, use);
}

// print indent, id and assign
// return true if in expression
inline bool PrintPrefix(std::ostream &os, IdManager &idm,
                        const Value *val) {
  if (!in_expr) os << kIndent;
  PrintId(os, idm, val);
  if (!in_expr) os << " = ";
  return in_expr;
}

}  // namespace

void LoadSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (PrintPrefix(os, idm, this)) return;
  auto inex = InExpr();
  os << "load ";
  PrintType(os, type());
  os << ", ";
  DumpWithType(os, idm, (*this)[0]);
  os << std::endl;
}

void StoreSSA::Dump(std::ostream &os, IdManager &idm) const {
  auto inex = InExpr();
  os << kIndent << "store ";
  DumpWithType(os, idm, (*this)[0]);
  os << ", ";
  DumpWithType(os, idm, (*this)[1]);
  os << std::endl;
}

void AccessSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (PrintPrefix(os, idm, this)) return;
  auto inex = InExpr();
  os << "access ";
  if (acc_type_ == AccessType::Pointer) {
    os << "ptr ";
  }
  else {
    os << "elem ";
  }
  DumpWithType(os, idm, (*this)[0]);
  os << ", ";
  DumpWithType(os, idm, (*this)[1]);
  os << std::endl;
}

void BinarySSA::Dump(std::ostream &os, IdManager &idm) const {
  if (PrintPrefix(os, idm, this)) return;
  auto inex = InExpr();
  os << kBinOps[static_cast<int>(op_)] << ' ';
  PrintType(os, type());
  os << ' ';
  DumpVal(os, idm, (*this)[0]);
  os << ", ";
  DumpVal(os, idm, (*this)[1]);
  os << std::endl;
}

void UnarySSA::Dump(std::ostream &os, IdManager &idm) const {
  if (PrintPrefix(os, idm, this)) return;
  auto inex = InExpr();
  os << kUnaOps[static_cast<int>(op_)] << ' ';
  PrintType(os, type());
  os << ' ';
  DumpVal(os, idm, (*this)[0]);
  os << std::endl;
}

void CallSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (PrintPrefix(os, idm, this)) return;
  auto inex = InExpr();
  os << "call ";
  DumpWithType(os, idm, (*this)[0].value());
  for (int i = 1; i < size(); ++i) {
    os << ", ";
    DumpVal(os, idm, (*this)[i]);
  }
  os << std::endl;
}

void BranchSSA::Dump(std::ostream &os, IdManager &idm) const {
  auto inex = InExpr();
  os << kIndent << "branch ";
  DumpVal(os, idm, (*this)[0]);
  os << ", ";
  DumpVal(os, idm, (*this)[1]);
  os << ", ";
  DumpVal(os, idm, (*this)[2]);
  os << std::endl;
}

void JumpSSA::Dump(std::ostream &os, IdManager &idm) const {
  auto inex = InExpr();
  os << kIndent << "jump ";
  DumpVal(os, idm, (*this)[0]);
  os << std::endl;
}

void ReturnSSA::Dump(std::ostream &os, IdManager &idm) const {
  auto inex = InExpr();
  os << kIndent << "return ";
  if (!(*this)[0].value()) {
    os << "void";
  }
  else {
    DumpWithType(os, idm, (*this)[0]);
  }
  os << std::endl;
}

void FunctionSSA::Dump(std::ostream &os, IdManager &idm) const {
  idm.LogName(this, name_);
  if (in_expr) {
    PrintId(os, idm, this);
    return;
  }
  if (size()) {
    os << "define ";
  }
  else {
    os << "declare ";
  }
  os << kLinkTypes[static_cast<int>(link_)] << ' ';
  PrintType(os, type());
  os << ' ';
  PrintId(os, idm, this);
  if (size()) {
    idm.ResetId();
    // log block name first
    {
      auto inex = InExpr();
      for (const auto &i : *this) DumpVal(null_os, idm, i);
    }
    // dump content of block
    os << " {" << std::endl;
    for (const auto &i : *this) DumpVal(os, idm, i);
    os << '}';
  }
  os << std::endl;
}

void GlobalVarSSA::Dump(std::ostream &os, IdManager &idm) const {
  idm.LogName(this, name_);
  PrintId(os, idm, this);
  if (in_expr) return;
  os << " = " << kLinkTypes[static_cast<int>(link_)] << " global ";
  PrintType(os, type());
  if ((*this)[0].value()) {
    auto inex = InExpr();
    os << ", ";
    DumpVal(os, idm, (*this)[0]);
  }
  os << std::endl;
}

void AllocaSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (PrintPrefix(os, idm, this)) return;
  auto inex = InExpr();
  os << "alloca ";
  PrintType(os, type());
  os << std::endl;
}

void BlockSSA::Dump(std::ostream &os, IdManager &idm) const {
  if (!name_.empty()) idm.LogName(this, name_);
  PrintId(os, idm, this);
  if (in_expr) return;
  os << ':';
  if (!preds_.empty()) {
    auto inex = InExpr();
    os << " ; preds: ";
    DumpVal(os, idm, preds_.begin(), preds_.end());
  }
  if (!succs_.empty() && succs_.front()) {
    auto inex = InExpr();
    os << " ; succs: ";
    DumpVal(os, idm, succs_.begin(), succs_.end());
  }
  os << std::endl;
  for (const auto &i : insts_) DumpVal(os, idm, i);
}

void ArgRefSSA::Dump(std::ostream &os, IdManager &idm) const {
  assert(in_expr);
  os << "arg " << index_;
}

void AsmSSA::Dump(std::ostream &os, IdManager &idm) const {
  os << kIndent << "asm \"";
  for (const auto &c : asm_str_) ConvertChar(os, c);
  os << '"' << std::endl;
}

void ConstIntSSA::Dump(std::ostream &os, IdManager &idm) const {
  assert(in_expr);
  os << "constant ";
  PrintType(os, type());
  os << ' ' << value_;
}

void ConstFloatSSA::Dump(std::ostream &os, IdManager &idm) const {
  assert(in_expr);
  os << "constant ";
  PrintType(os, type());
  os << ' ' << value_;
}

void ConstStrSSA::Dump(std::ostream &os, IdManager &idm) const {
  assert(in_expr);
  os << "constant ";
  PrintType(os, type());
  os << " \"";
  for (const auto &c : str_) ConvertChar(os, c);
  os << '"';
}

void ConstStructSSA::Dump(std::ostream &os, IdManager &idm) const {
  assert(in_expr);
  os << "constant ";
  PrintType(os, type());
  os << " {";
  for (int i = 0; i < size(); ++i) {
    if (i) os << ", ";
    DumpVal(os, idm, (*this)[i]);
  }
  os << '}';
}

void ConstArraySSA::Dump(std::ostream &os, IdManager &idm) const {
  assert(in_expr);
  os << "constant ";
  PrintType(os, type());
  os << " {";
  for (int i = 0; i < size(); ++i) {
    if (i) os << ", ";
    DumpVal(os, idm, (*this)[i]);
  }
  os << '}';
}

void ConstZeroSSA::Dump(std::ostream &os, IdManager &idm) const {
  assert(in_expr);
  os << "constant ";
  PrintType(os, type());
  os << " zero";
}
