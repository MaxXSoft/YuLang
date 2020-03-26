#include "define/ast.h"

#include <iomanip>
#include <cctype>
#include <cassert>

#include "xstl/guard.h"

using namespace yulang::define;

namespace {

const char *kOperators[] = {YULANG_OPERATORS(YULANG_EXPAND_SECOND)};

int indent_count = 0, in_expr = 0;

const auto indent = [](std::ostream &os) {
  if (indent_count && !in_expr) {
    os << std::setw(indent_count * 2) << std::setfill(' ') << ' ';
  }
};

std::ostream &operator<<(std::ostream &os, decltype(indent) func) {
  func(os);
  return os;
}

xstl::Guard Indent() {
  ++indent_count;
  return xstl::Guard([] { --indent_count; });
}

xstl::Guard InExpr() {
  ++in_expr;
  return xstl::Guard([] { --in_expr; });
}

xstl::Guard InExpr(std::ostream &os) {
  os << indent;
  ++in_expr;
  return xstl::Guard([&os] {
    --in_expr;
    if (!in_expr) os << ';' << std::endl;
  });
}

void ConvertChar(std::ostream &os, char c, bool in_char) {
  switch (c) {
    case '\a':  os << "\\a";                      break;
    case '\b':  os << "\\b";                      break;
    case '\f':  os << "\\f";                      break;
    case '\n':  os << "\\n";                      break;
    case '\r':  os << "\\r";                      break;
    case '\t':  os << "\\t";                      break;
    case '\v':  os << "\\v'";                     break;
    case '\\':  os << "\\\\";                     break;
    case '\'':  os << (in_char ? "\\'" : "'");    break;
    case '"':   os << (in_char ? "\'" : "\\\"");  break;
    case '\0':  os << "\\0";                      break;
    default: {
      if (std::isprint(c)) {
        os << c;
      }
      else {
        os << "\\x" << std::setw(2) << std::setfill('0') << std::hex
           << static_cast<int>(c);
      }
    }
  }
}

void DumpProperty(std::ostream &os, Property prop) {
  if (!indent_count) {
    os << indent;
    if (prop == Property::None) {
      os << "static ";
    }
    else if (prop == Property::Extern) {
      os << "extern \"C\" ";
    }
    else if (prop == Property::Inline) {
      os << "inline ";
    }
  }
}

}  // namespace

void VarLetDefAST::Dump(std::ostream &os) {
  for (const auto &i : defs_) {
    DumpProperty(os, prop_);
    os << indent;
    i->Dump(os);
  }
}

void FunDefAST::Dump(std::ostream &os) {
  DumpProperty(os, prop_);
  os << indent;
  if (type_) {
    type_->Dump(os);
  }
  else {
    os << "void";
  }
  os << ' ' << id_ << '(';
  for (int i = 0; i < args_.size(); ++i) {
    if (i) os << ", ";
    args_[i]->Dump(os);
  }
  os << ')' << std::endl;
  if (body_) body_->Dump(os);
}

void DeclareAST::Dump(std::ostream &os) {
  DumpProperty(os, prop_);
  os << indent;
  if (!is_var_) os << "const ";
  type_->Dump(os);
  os << ' ' << id_ << ';' << std::endl;
}

void TypeAliasAST::Dump(std::ostream &os) {
  DumpProperty(os, prop_);
  os << indent << "using " << id_ << " = ";
  type_->Dump(os);
  os << ';' << std::endl;
}

void StructAST::Dump(std::ostream &os) {
  DumpProperty(os, prop_);
  os << indent << "struct " << id_ << " {" << std::endl;
  {
    auto ind = Indent();
    for (const auto &i : defs_) {
      os << indent;
      i->Dump(os);
      os << ';' << std::endl;
    }
  }
  os << indent << "};" << std::endl;
}

void EnumAST::Dump(std::ostream &os) {
  DumpProperty(os, prop_);
  os << indent << "enum class " << id_;
  if (type_) {
    os << " : ";
    type_->Dump(os);
  }
  os << " {" << std::endl;
  {
    auto ind = Indent();
    for (const auto &i : defs_) {
      os << indent;
      i->Dump(os);
      os << ',' << std::endl;
    }
  }
  os << indent << "};" << std::endl;
}

void ImportAST::Dump(std::ostream &os) {
  os << indent << "/*" << std::endl;
  {
    auto ind = Indent();
    os << indent << "imported contents:\n" << std::endl;
    for (const auto &i : defs_) {
      i->Dump(os);
      os << std::endl;
    }
  }
  os << indent << "*/" << std::endl;
}

void VarLetElemAST::Dump(std::ostream &os) {
  if (type_) {
    type_->Dump(os);
  }
  else {
    os << "auto";
  }
  os << ' ' << id_;
  if (init_) {
    os << " = ";
    auto inex = InExpr();
    init_->Dump(os);
  }
  os << ';' << std::endl;
}

void ArgElemAST::Dump(std::ostream &os) {
  type_->Dump(os);
  os << ' ' << id_;
}

void StructElemAST::Dump(std::ostream &os) {
  type_->Dump(os);
  os << ' ' << id_;
}

void EnumElemAST::Dump(std::ostream &os) {
  os << id_;
  if (expr_) {
    os << " = ";
    auto inex = InExpr();
    expr_->Dump(os);
  }
}

void BlockAST::Dump(std::ostream &os) {
  os << indent << '{' << std::endl;
  {
    auto ind = Indent();
    for (const auto &i : stmts_) i->Dump(os);
  }
  os << indent << '}' << std::endl;
}

void IfAST::Dump(std::ostream &os) {
  os << indent << "if (";
  {
    auto inex = InExpr();
    cond_->Dump(os);
  }
  os << ')' << std::endl;
  auto prev_inex = in_expr;
  if (in_expr) in_expr = 0;
  then_->Dump(os);
  if (else_then_) {
    os << indent << "else" << std::endl;
    else_then_->Dump(os);
  }
  in_expr = prev_inex;
}

void WhenAST::Dump(std::ostream &os) {
  os << indent << "switch (";
  {
    auto inex = InExpr();
    expr_->Dump(os);
  }
  os << ") {" << std::endl;
  auto prev_inex = in_expr;
  if (in_expr) in_expr = 0;
  {
    auto ind = Indent();
    for (const auto &i : elems_) {
      i->Dump(os);
    }
    if (else_then_) {
      os << indent << "default:" << std::endl;
      auto ind = Indent();
      else_then_->Dump(os);
      os << indent << "break;" << std::endl;
    }
  }
  os << indent << '}' << std::endl;
  in_expr = prev_inex;
}

void WhileAST::Dump(std::ostream &os) {
  os << indent << "while (";
  {
    auto inex = InExpr();
    cond_->Dump(os);
  }
  os << ')' << std::endl;
  body_->Dump(os);
}

void ForInAST::Dump(std::ostream &os) {
  os << indent << "for (const auto &" << id_ << " : ";
  {
    auto inex = InExpr();
    expr_->Dump(os);
  }
  os << ')' << std::endl;
  body_->Dump(os);
}

void AsmAST::Dump(std::ostream &os) {
  os << indent << "asm volatile(\"";
  for (const auto &c : asm_str_) ConvertChar(os, c, false);
  os << "\");" << std::endl;
}

void ControlAST::Dump(std::ostream &os) {
  os << indent;
  switch (type_) {
    case Keyword::Break: os << "break"; break;
    case Keyword::Continue: os << "continue"; break;
    case Keyword::Return: {
      os << "return";
      if (expr_) {
        auto inex = InExpr();
        os << ' ';
        expr_->Dump(os);
      }
      break;
    }
    default: assert(false);
  }
  os << ';' << std::endl;
}

void WhenElemAST::Dump(std::ostream &os) {
  for (const auto &i : conds_) {
    os << indent << "case ";
    auto inex = InExpr();
    i->Dump(os);
    os << ':' << std::endl;
  }
  auto ind = Indent();
  body_->Dump(os);
  os << indent << "break;" << std::endl;
}

void BinaryAST::Dump(std::ostream &os) {
  os << indent;
  if (in_expr) os << '(';
  {
    auto inex = InExpr();
    lhs_->Dump(os);
    if (op_ != Operator::Access) os << ' ';
    os << kOperators[static_cast<int>(op_)];
    if (op_ != Operator::Access) os << ' ';
    rhs_->Dump(os);
  }
  if (!in_expr) {
    os << ';' << std::endl;
  }
  else {
    os << ')';
  }
}

void AccessAST::Dump(std::ostream &os) {
  auto inex = InExpr();
  expr_->Dump(os);
  os << '.' << id_;
}

void CastAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  os << '(';
  type_->Dump(os);
  os << ") ";
  expr_->Dump(os);
}

void UnaryAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  switch (op_) {
    case UnaryOp::Pos:      os << '+';        break;
    case UnaryOp::Neg:      os << '-';        break;
    case UnaryOp::LogicNot: os << '!';        break;
    case UnaryOp::Not:      os << '~';        break;
    case UnaryOp::DeRef:    os << '*';        break;
    case UnaryOp::AddrOf:   os << '&';        break;
    case UnaryOp::SizeOf:   os << "sizeof ";  break;
    default: assert(false);
  }
  opr_->Dump(os);
}

void IndexAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  expr_->Dump(os);
  os << '[';
  index_->Dump(os);
  os << ']';
}

void FunCallAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  expr_->Dump(os);
  os << '(';
  for (int i = 0; i < args_.size(); ++i) {
    if (i) os << ", ";
    args_[i]->Dump(os);
  }
  os << ')';
}

void IntAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  os << value_;
}

void FloatAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  os << value_;
}

void CharAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  os << "'";
  ConvertChar(os, c_, true);
  os << "'";
}

void IdAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  os << id_;
}

void StringAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  os << '"';
  for (const auto &c : str_) ConvertChar(os, c, false);
  os << '"';
}

void BoolAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  os << std::boolalpha << value_;
}

void NullAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  os << "nullptr";
}

void ValInitAST::Dump(std::ostream &os) {
  auto inex = InExpr(os);
  type_->Dump(os);
  os << " {";
  for (int i = 0; i < elems_.size(); ++i) {
    if (i) os << ", ";
    elems_[i]->Dump(os);
  }
  os << '}';
}

void PrimTypeAST::Dump(std::ostream &os) {
  switch (type_) {
    case Keyword::Int8:     os << "int8_t";   break;
    case Keyword::Int16:    os << "int16_t";  break;
    case Keyword::Int32:    os << "int32_t";  break;
    case Keyword::Int64:    os << "int64_t";  break;
    case Keyword::UInt8:    os << "uint8_t";  break;
    case Keyword::UInt16:   os << "uint16_t"; break;
    case Keyword::UInt32:   os << "uint32_t"; break;
    case Keyword::UInt64:   os << "uint64_t"; break;
    case Keyword::Float32:  os << "float";    break;
    case Keyword::Float64:  os << "double";   break;
    case Keyword::Bool:     os << "bool";     break;
    default: assert(false);
  }
}

void UserTypeAST::Dump(std::ostream &os) {
  os << id_;
}

void FuncTypeAST::Dump(std::ostream &os) {
  if (ret_) {
    ret_->Dump(os);
  }
  else {
    os << "void";
  }
  os << " (*)(";
  for (int i = 0; i < args_.size(); ++i) {
    if (i) os << ", ";
    args_[i]->Dump(os);
  }
  os << ')';
}

void VolaTypeAST::Dump(std::ostream &os) {
  type_->Dump(os);
  os << " volatile";
}

void ArrayTypeAST::Dump(std::ostream &os) {
  base_->Dump(os);
  os << '[';
  {
    auto inex = InExpr();
    expr_->Dump(os);
  }
  os << ']';
}

void PointerTypeAST::Dump(std::ostream &os) {
  base_->Dump(os);
  os << '*';
  if (!is_var_) os << "const";
}

void RefTypeAST::Dump(std::ostream &os) {
  base_->Dump(os);
  os << '&';
  if (!is_var_) os << "const";
}
