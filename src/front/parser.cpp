#include "front/parser.h"

#include <sstream>
#include <stack>

#include "define/token.h"

using namespace yulang::front;
using namespace yulang::define;

namespace {

// type aliases
using Prop = PropertyAST::Property;
using UnaryOp = UnaryAST::UnaryOp;

// table of operator's precedence
// -1 if it's not a binary/assign operator
const int kOpPrecTable[] = {YULANG_OPERATORS(YULANG_EXPAND_THIRD)};

// table of operator's name
const char *kOperators[] = {YULANG_OPERATORS(YULANG_EXPAND_SECOND)};

// return precedence of specific operator
inline int GetOpPrec(Operator op) {
  return kOpPrecTable[static_cast<int>(op)];
}

}  // namespace

ASTPtr Parser::LogError(std::string_view message) {
  logger().LogError(message);
  return nullptr;
}

ASTPtr Parser::ParseLine() {
  auto stmt = GetStatement(GetProp());
  if (!stmt) return LogError("invalid statement");
  if (!ExpectEOL()) return nullptr;
  return stmt;
}

ASTPtr Parser::ParseVarDef(ASTPtr prop) {
  auto log = logger();
  NextToken();
  if (!ExpectId()) return nullptr;
  // get definition list
  ASTPtrList defs;
  for (;;) {
    auto elem = ParseVarElem();
    if (!elem) return nullptr;
    defs.push_back(std::move(elem));
    // eat ','
    if (!IsTokenChar(',')) break;
    NextToken();
  }
  return MakeAST<VarLetDefAST>(log, std::move(prop), std::move(defs));
}

ASTPtr Parser::ParseLetDef(ASTPtr prop) {
  auto log = logger();
  NextToken();
  if (!ExpectId()) return nullptr;
  // get definition list
  ASTPtrList defs;
  for (;;) {
    auto elem = ParseLetElem();
    if (!elem) return nullptr;
    defs.push_back(std::move(elem));
    // eat ','
    if (!IsTokenChar(',')) break;
    NextToken();
  }
  return MakeAST<VarLetDefAST>(log, std::move(prop), std::move(defs));
}

ASTPtr Parser::ParseFunDef(ASTPtr prop) {
  auto log = logger();
  NextToken();
  // get function name
  std::string name;
  if (cur_token_ == Token::Id) {
    name = lexer()->id_val();
  }
  else if (cur_token_ == Token::Operator) {
    name = kOperators[static_cast<int>(lexer()->op_val())];
  }
  else {
    return LogError("invalid function name");
  }
  NextToken();
  // check & eat '('
  if (!ExpectChar('(')) return nullptr;
  // get argument list
  ASTPtrList args;
  if (!IsTokenChar(')')) {
    // parse all arguments definitions
    for (;;) {
      auto arg = ParseArgElem();
      if (!arg) return nullptr;
      args.push_back(std::move(arg));
      // eat ','
      if (!IsTokenChar(',')) break;
      NextToken();
    }
  }
  // check & eat ')'
  if (!ExpectChar(')')) return nullptr;
  // check & get return type
  ASTPtr type;
  if (IsTokenChar(':')) {
    NextToken();
    type = ParseType();
    if (!type) return nullptr;
  }
  // get function body
  auto body = ParseBlock();
  if (!body) return nullptr;
  return MakeAST<FunDefAST>(log, std::move(prop), name, std::move(args),
                            std::move(type), std::move(body));
}

ASTPtr Parser::ParseDeclare(ASTPtr prop) {
  auto log = logger();
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // check & eat ':'
  if (!ExpectChar(':')) return nullptr;
  // get type
  auto type = ParseType();
  return MakeAST<DeclareAST>(log, std::move(prop), id, std::move(type));
}

ASTPtr Parser::ParseTypeAlias(ASTPtr prop) {
  auto log = logger();
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // check & eat '='
  if (!IsTokenOperator(Operator::Assign)) return LogError("expected '='");
  // get type
  auto type = ParseType();
  return MakeAST<TypeAliasAST>(log, std::move(prop), id, std::move(type));
}

ASTPtr Parser::ParseStruct(ASTPtr prop) {
  auto log = logger();
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // check & eat '{'
  if (!ExpectChar('{')) return nullptr;
  // get definitions of all fields
  ASTPtrList defs;
  while (cur_token_ == Token::Id) {
    auto elem = ParseArgElem();
    if (!elem) return nullptr;
    defs.push_back(std::move(elem));
    // eat ','
    if (!IsTokenChar(',')) break;
    NextToken();
  }
  // check & eat '}'
  if (!ExpectChar('}')) return nullptr;
  return MakeAST<StructAST>(log, std::move(prop), id, std::move(defs));
}

ASTPtr Parser::ParseEnum(ASTPtr prop) {
  auto log = logger();
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // get type
  ASTPtr type;
  if (IsTokenChar(':')) {
    NextToken();
    type = ParseType();
    if (!type) return nullptr;
  }
  // check & eat '{'
  if (!ExpectChar('{')) return nullptr;
  // get enumeration list
  ASTPtrList defs;
  while (cur_token_ == Token::Id) {
    auto elem = ParseEnumElem();
    if (!elem) return nullptr;
    defs.push_back(std::move(elem));
    // eat ','
    if (!IsTokenChar(',')) break;
    NextToken();
  }
  // check & eat '}'
  if (!ExpectChar('}')) return nullptr;
  return MakeAST<EnumAST>(log, std::move(prop), id, std::move(type),
                          std::move(defs));
}

ASTPtr Parser::ParseImport() {
  auto log = logger();
  NextToken();
  // get module name
  ModName mod_name;
  for (;;) {
    if (!ExpectId()) return nullptr;
    mod_name.emplace_back(lexer()->id_val());
    NextToken();
    // eat '.'
    if (!IsTokenOperator(Operator::Access)) break;
    NextToken();
  }
  // switch lexer
  auto last_token = last_token_, cur_token = cur_token_;
  auto last_lex = lex_man_.SetLexer(mod_name);
  if (!last_lex) return LogError("invalid module name");
  NextToken();
  // get all public/extern definitions
  ASTPtrList defs;
  while (!logger().error_num() && cur_token_ != Token::End) {
    auto prop = GetProp();
    if (prop != Prop::None) {
      prop = prop == Prop::Extern ? Prop::Demangle : Prop::None;
      auto def = GetStatement(prop);
      if (!def) return nullptr;
      defs.push_back(std::move(def));
    }
    else {
      NextToken();
    }
  }
  // reset to original status
  lex_man_.SetLexer(last_lex);
  last_token_ = last_token;
  cur_token_ = cur_token;
  return MakeAST<ImportAST>(log, std::move(defs));
}

ASTPtr Parser::ParseVarElem() {
  auto log = logger();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // get type
  ASTPtr type;
  if (IsTokenChar(':')) {
    NextToken();
    type = ParseType();
    if (!type) return nullptr;
  }
  // get initialization expression
  ASTPtr init;
  if (IsTokenOperator(Operator::Assign)) {
    NextToken();
    init = ParseExpr();
    if (!init) return nullptr;
  }
  // check type & init
  if (!type && !init) return LogError("initializer required");
  return MakeAST<VarElemAST>(log, id, std::move(type), std::move(init));
}

ASTPtr Parser::ParseLetElem() {
  auto log = logger();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // get type
  ASTPtr type;
  if (IsTokenChar(':')) {
    NextToken();
    type = ParseType();
    if (!type) return nullptr;
  }
  // get initialization expression
  if (!IsTokenOperator(Operator::Assign)) {
    return LogError("expected initialization expression");
  }
  NextToken();
  auto init = ParseExpr();
  if (!init) return nullptr;
  return MakeAST<LetElemAST>(log, id, std::move(type), std::move(init));
}

ASTPtr Parser::ParseArgElem() {
  auto log = logger();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // check ':'
  if (!ExpectChar(':')) return nullptr;
  // get type
  auto type = ParseType();
  if (!type) return nullptr;
  return MakeAST<ArgElemAST>(log, id, std::move(type));
}

ASTPtr Parser::ParseEnumElem() {
  auto log = logger();
  // get identifier
  auto id = lexer()->id_val();
  NextToken();
  // get expression
  ASTPtr expr;
  if (IsTokenOperator(Operator::Assign)) {
    NextToken();
    expr = ParseExpr();
    if (!expr) return nullptr;
  }
  return MakeAST<EnumElemAST>(log, id, std::move(expr));
}

ASTPtr Parser::ParseBlock() {
  auto log = logger();
  // check & eat '{'
  if (!ExpectChar('{')) return nullptr;
  // get lines
  ASTPtrList stmts;
  while (!IsTokenChar('}')) {
    auto stmt = ParseBlockLine();
    if (!stmt) return nullptr;
    stmts.push_back(std::move(stmt));
  }
  // eat '}'
  NextToken();
  return MakeAST<BlockAST>(log, std::move(stmts));
}

ASTPtr Parser::ParseBlockLine() {
  auto stmt = ParseBlockStatement();
  if (!ExpectEOL()) return nullptr;
  return stmt;
}

ASTPtr Parser::ParseBlockStatement() {
  // get normal statement
  auto stmt = GetStatement(Prop::None);
  if (stmt) return stmt;
  // parse other in-block statements
  if (cur_token_ == Token::Keyword) {
    switch (lexer()->key_val()) {
      case Keyword::While: return ParseWhile();
      case Keyword::For: return ParseForIn();
      case Keyword::Asm: return ParseAsm();
      case Keyword::Break: case Keyword::Continue:
      case Keyword::Return: return ParseControl();
      default:;
    }
  }
  // parse expression
  return ParseExpr();
}

ASTPtr Parser::ParseIfElse() {
  auto log = logger();
  NextToken();
  // get condition expression
  auto cond = ParseExpr();
  if (!cond) return nullptr;
  // get then body
  auto then = ParseBlock();
  if (!then) return nullptr;
  // get else then body
  ASTPtr else_then;
  if (IsTokenKeyword(Keyword::Else)) {
    NextToken();
    else_then = IsTokenKeyword(Keyword::If) ? ParseIfElse() : ParseBlock();
    if (!else_then) return nullptr;
  }
  return MakeAST<IfAST>(log, std::move(cond), std::move(then),
                        std::move(else_then));
}

ASTPtr Parser::ParseWhen() {
  auto log = logger();
  NextToken();
  // get condition expression
  auto cond = ParseExpr();
  if (!cond) return nullptr;
  // check & eat '{'
  if (!ExpectChar('{')) return nullptr;
  // get when body
  ASTPtrList elems;
  while (!IsTokenKeyword(Keyword::Else) && !IsTokenChar('}')) {
    auto elem = ParseWhenElem();
    if (!elem) return nullptr;
    elems.push_back(std::move(elem));
  }
  if (elems.empty()) {
    return LogError("'when' statement must have at least one case");
  }
  // get else then body
  ASTPtr else_then;
  if (IsTokenKeyword(Keyword::Else)) {
    NextToken();
    else_then = ParseBlock();
    if (!else_then) return nullptr;
  }
  // check & eat '}'
  if (!ExpectChar('}')) return nullptr;
  return MakeAST<WhenAST>(log, std::move(cond), std::move(elems),
                          std::move(else_then));
}

ASTPtr Parser::ParseWhile() {
  auto log = logger();
  NextToken();
  // get condition expression
  auto cond = ParseExpr();
  if (!cond) return nullptr;
  // get body
  auto body = ParseBlock();
  if (!body) return nullptr;
  return MakeAST<WhileAST>(log, std::move(cond), std::move(body));
}

ASTPtr Parser::ParseForIn() {
  auto log = logger();
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // check 'in'
  if (!IsTokenKeyword(Keyword::In)) return LogError("expected 'in'");
  NextToken();
  // get expression
  auto expr = ParseExpr();
  if (!expr) return nullptr;
  // get body
  auto body = ParseBlock();
  if (!body) return nullptr;
  return MakeAST<ForInAST>(log, id, std::move(expr), std::move(body));
}

ASTPtr Parser::ParseAsm() {
  auto log = logger();
  NextToken();
  // check & eat '{'
  if (!ExpectChar('{')) return nullptr;
  // get string list
  std::ostringstream oss;
  while (cur_token_ == Token::String) {
    oss << lexer()->str_val() << std::endl;
  }
  // check & eat '}'
  if (!ExpectChar('}')) return nullptr;
  return MakeAST<AsmAST>(log, oss.str());
}

ASTPtr Parser::ParseControl() {
  auto log = logger();
  // get keyword type
  auto type = lexer()->key_val();
  NextToken();
  // check return expression
  ASTPtr expr;
  if (type == Keyword::Return && last_token_ != Token::EOL) {
    expr = ParseExpr();
    if (!expr) return nullptr;
  }
  return MakeAST<ControlAST>(log, type, std::move(expr));
}

ASTPtr Parser::ParseWhenElem() {
  auto log = logger();
  // get condition list
  ASTPtrList conds;
  for (;;) {
    auto expr = ParseExpr();
    if (!expr) return nullptr;
    conds.push_back(std::move(expr));
    // check ','
    if (!IsTokenChar(',')) break;
    NextToken();
  }
  // get body
  auto body = ParseBlock();
  if (!body) return nullptr;
  return MakeAST<WhenElemAST>(log, std::move(conds), std::move(body));
}

ASTPtr Parser::ParseExpr() {
  auto log = logger();
  // get the first binary expression
  auto lhs = ParseBinary();
  if (!lhs) return nullptr;
  // try to get the rest binary expressions
  while (cur_token_ == Token::Id && last_token_ != Token::EOL) {
    auto id = ParseId();
    auto rhs = ParseBinary();
    if (!rhs) return nullptr;
    // make function call
    ASTPtrList args;
    args.push_back(std::move(lhs));
    args.push_back(std::move(rhs));
    lhs = MakeAST<FunCallAST>(log, std::move(id), std::move(args));
  }
  return lhs;
}

ASTPtr Parser::ParseBinary() {
  auto log = logger();
  std::stack<ASTPtr> oprs;
  std::stack<Operator> ops;
  // get the first expression
  auto expr = ParseCast();
  if (!expr) return nullptr;
  oprs.push(std::move(expr));
  // convert to postfix expression
  while (cur_token_ == Token::Operator) {
    // get operator
    auto op = lexer()->op_val();
    if (GetOpPrec(op) < 0) break;
    NextToken();
    // handle operator
    while (!ops.empty() && GetOpPrec(ops.top()) >= GetOpPrec(op)) {
      // create a new binary AST
      auto cur_op = ops.top();
      ops.pop();
      auto rhs = std::move(oprs.top());
      oprs.pop();
      auto lhs = std::move(oprs.top());
      oprs.pop();
      oprs.push(MakeAST<BinaryAST>(log, cur_op, std::move(lhs),
                                   std::move(rhs)));
    }
    ops.push(op);
    // get next expression
    expr = ParseCast();
    if (!expr) return nullptr;
    oprs.push(std::move(expr));
  }
  // clear stacks
  while (!ops.empty()) {
    // create a new binary AST
    auto cur_op = ops.top();
    ops.pop();
    auto rhs = std::move(oprs.top());
    oprs.pop();
    auto lhs = std::move(oprs.top());
    oprs.pop();
    oprs.push(MakeAST<BinaryAST>(log, cur_op, std::move(lhs),
                                 std::move(rhs)));
  }
  return std::move(oprs.top());
}

ASTPtr Parser::ParseCast() {
  auto log = logger();
  // get unary expression
  auto expr = ParseUnary();
  if (!expr) return nullptr;
  // check if need to cast
  if (IsTokenKeyword(Keyword::As)) {
    NextToken();
    auto type = ParseType();
    if (!type) return nullptr;
    // create a new cast AST
    expr = MakeAST<CastAST>(log, std::move(expr), std::move(type));
  }
  return expr;
}

ASTPtr Parser::ParseUnary() {
  auto log = logger();
  UnaryOp una_op;
  // check if need to get operator
  if (cur_token_ == Token::Operator) {
    // get & check unary operator
    auto op = lexer()->op_val();
    switch (op) {
      case Operator::Add: una_op = UnaryOp::Pos; break;
      case Operator::Sub: una_op = UnaryOp::Neg; break;
      case Operator::LogicNot: una_op = UnaryOp::LogicNot; break;
      case Operator::Not: una_op = UnaryOp::Not; break;
      case Operator::Mul: una_op = UnaryOp::DeRef; break;
      case Operator::And: una_op = UnaryOp::AddrOf; break;
      default: return LogError("invalid unary operator");
    }
  }
  else if (IsTokenKeyword(Keyword::SizeOf)) {
    // treat 'sizeof' as an unary operator
    una_op = UnaryOp::SizeOf;
  }
  else {
    return ParseFactor();
  }
  NextToken();
  // get factor
  auto expr = ParseFactor();
  if (!expr) return nullptr;
  return MakeAST<UnaryAST>(log, una_op, std::move(expr));
}

ASTPtr Parser::ParseFactor() {
  ASTPtr factor;
  if (cur_token_ == Token::Keyword) {
    // if/when expression
    switch (lexer()->key_val()) {
      case Keyword::If: factor = ParseIfElse(); break;
      case Keyword::When: factor = ParseWhen(); break;
      default: return LogError("invalid factor");
    }
  }
  else if (IsTokenChar('{')) {
    // block
    factor = ParseBlock();
  }
  else if (IsTokenChar('(')) {
    // bracket expression
    NextToken();
    factor = ParseExpr();
    if (!ExpectChar(')')) return nullptr;
  }
  else {
    // other values
    factor = ParseValue();
  }
  // parse the rest part
  while (factor && last_token_ != Token::EOL) {
    if (IsTokenChar('[')) {
      factor = ParseIndex(std::move(factor));
    }
    else if (IsTokenChar('(')) {
      factor = ParseFunCall(std::move(factor));
    }
    else {
      break;
    }
  }
  return factor;
}

ASTPtr Parser::ParseIndex(ASTPtr expr) {
  auto log = logger();
  // eat '['
  NextToken();
  // get expression
  auto index = ParseExpr();
  if (!index || !ExpectChar(']')) return nullptr;
  return MakeAST<IndexAST>(log, std::move(expr), std::move(index));
}

ASTPtr Parser::ParseFunCall(ASTPtr expr) {
  auto log = logger();
  // eat '('
  NextToken();
  // get expression list
  ASTPtrList args;
  if (!IsTokenChar(')')) {
    for (;;) {
      auto arg = ParseExpr();
      if (!arg) return nullptr;
      args.push_back(std::move(arg));
      // eat ','
      if (!IsTokenChar(',')) break;
      NextToken();
    }
  }
  // check & eat ')'
  if (!ExpectChar(')')) return nullptr;
  return MakeAST<FunCallAST>(log, std::move(expr), std::move(args));
}

ASTPtr Parser::ParseValue() {
  switch (cur_token_) {
    case Token::Int: return ParseInt();
    case Token::Float: return ParseFloat();
    case Token::Char: return ParseChar();
    case Token::Id: return ParseId();
    case Token::String: return ParseString();
    case Token::Keyword: {
      switch (lexer()->key_val()) {
        case Keyword::True: case Keyword::False: return ParseBool();
        case Keyword::Null: return ParseNull();
        default: return ParseValInit();
      }
    }
    default: {
      if (IsTokenChar('[')) {
        return ParseValInit();
      }
      else {
        return LogError("invalid value");
      }
    }
  }
}

ASTPtr Parser::ParseInt() {
  auto ast = MakeAST<IntAST>(lexer()->int_val());
  NextToken();
  return ast;
}

ASTPtr Parser::ParseFloat() {
  auto ast = MakeAST<FloatAST>(lexer()->fp_val());
  NextToken();
  return ast;
}

ASTPtr Parser::ParseChar() {
  auto ast = MakeAST<CharAST>(lexer()->char_val());
  NextToken();
  return ast;
}

ASTPtr Parser::ParseId() {
  auto ast = MakeAST<IdAST>(lexer()->id_val());
  NextToken();
  return ast;
}

ASTPtr Parser::ParseString() {
  auto ast = MakeAST<StringAST>(lexer()->str_val());
  NextToken();
  return ast;
}

ASTPtr Parser::ParseBool() {
  auto ast = MakeAST<BoolAST>(lexer()->key_val() == Keyword::True);
  NextToken();
  return ast;
}

ASTPtr Parser::ParseNull() {
  auto ast = MakeAST<NullAST>();
  NextToken();
  return ast;
}

ASTPtr Parser::ParseValInit() {
  auto log = logger();
  NextToken();
  // get type
  auto type = ParseType();
  if (!type) return nullptr;
  // check & eat ']'
  if (!ExpectChar(']')) return nullptr;
  // check & eat '{'
  if (!ExpectChar('{')) return nullptr;
  // get elements
  ASTPtrList elems;
  while (!IsTokenChar('}')) {
    // get next expression
    auto elem = ParseExpr();
    if (!elem) return nullptr;
    elems.push_back(std::move(elem));
    // check comma
    if (!IsTokenChar(',')) break;
    NextToken();
  }
  // check & eat '}'
  if (!ExpectChar('}')) return nullptr;
  return MakeAST<ValInitAST>(log, std::move(type), std::move(elems));
}

ASTPtr Parser::ParseType() {
  // get type
  auto type = ParseValType();
  if (!type) return nullptr;
  // check if is volatiled type
  type = ParseVolaType(std::move(type));
  // parse the rest
  while (last_token_ != Token::EOL) {
    if (IsTokenChar('[')) {
      // array type
      type = ParseArray(std::move(type));
    }
    else {
      // check if is a variable reference
      auto is_var = false;
      if (IsTokenKeyword(Keyword::Var)) {
        is_var = true;
        NextToken();
      }
      if (IsTokenOperator(Operator::Mul)) {
        // pointer type
        type = ParsePointer(is_var, std::move(type));
      }
      else if (IsTokenOperator(Operator::And)) {
        // reference type
        type = ParseRef(is_var, std::move(type));
      }
      else {
        break;
      }
    }
    // check if is volatiled type
    if (!type) return nullptr;
    type = ParseVolaType(std::move(type));
  }
  return type;
}

ASTPtr Parser::ParseValType() {
  if (IsTokenChar('(')) {
    // function type
    return ParseFunc();
  }
  else if (cur_token_ == Token::Keyword) {
    // primitive type
    return ParsePrimType();
  }
  else if (cur_token_ == Token::Id) {
    // user defined type
    auto type = MakeAST<UserTypeAST>(lexer()->id_val());
    NextToken();
    return type;
  }
  else {
    return LogError("invalid value type");
  }
}

ASTPtr Parser::ParsePrimType() {
  switch (lexer()->key_val()) {
    case Keyword::Int8: case Keyword::Int16: case Keyword::Int32:
    case Keyword::Int64: case Keyword::UInt8: case Keyword::UInt16:
    case Keyword::UInt32: case Keyword::UInt64: case Keyword::Float32:
    case Keyword::Float64: case Keyword::Bool: break;
    default: return LogError("expected primitive type keywords");
  }
  auto ast = MakeAST<PrimTypeAST>(lexer()->key_val());
  NextToken();
  return ast;
}

ASTPtr Parser::ParseFunc() {
  auto log = logger();
  // eat '('
  NextToken();
  // get type list
  ASTPtrList args;
  if (!IsTokenChar(')')) {
    for (;;) {
      auto arg = ParseType();
      if (!arg) return nullptr;
      args.push_back(std::move(arg));
      // check ','
      if (!IsTokenChar(',')) break;
      NextToken();
    }
  }
  // check & eat ')'
  if (!ExpectChar(')')) return nullptr;
  // get return type
  ASTPtr ret;
  if (IsTokenChar(':')) {
    NextToken();
    ret = ParseType();
    if (!ret) return nullptr;
  }
  return MakeAST<FuncTypeAST>(log, std::move(args), std::move(ret));
}

ASTPtr Parser::ParseVolaType(ASTPtr type) {
  if (IsTokenKeyword(Keyword::Volatile)) {
    type = MakeAST<VolaTypeAST>(std::move(type));
    NextToken();
  }
  return type;
}

ASTPtr Parser::ParseArray(ASTPtr type) {
  auto log = logger();
  // eat '['
  NextToken();
  // get dimension expression
  auto expr = ParseExpr();
  if (!expr) return nullptr;
  // check & eat ']'
  if (!ExpectChar(']')) return nullptr;
  return MakeAST<ArrayTypeAST>(log, std::move(type), std::move(expr));
}

ASTPtr Parser::ParsePointer(bool is_var, ASTPtr type) {
  auto ast = MakeAST<PointerTypeAST>(is_var, std::move(type));
  NextToken();
  return ast;
}

ASTPtr Parser::ParseRef(bool is_var, ASTPtr type) {
  auto ast = MakeAST<RefTypeAST>(is_var, std::move(type));
  NextToken();
  return ast;
}

Prop Parser::GetProp() {
  if (IsTokenKeyword(Keyword::Public)) {
    // eat 'public'
    NextToken();
    return Prop::Public;
  }
  else if (IsTokenKeyword(Keyword::Extern)) {
    // eat 'extern'
    NextToken();
    return Prop::Extern;
  }
  return Prop::None;
}

ASTPtr Parser::GetStatement(Prop prop) {
  auto prop_ast = MakeAST<PropertyAST>(prop);
  // parse definitions and declarations
  if (cur_token_ != Token::Keyword) return nullptr;
  switch (lexer()->key_val()) {
    case Keyword::Var: return ParseVarDef(std::move(prop_ast));
    case Keyword::Let: return ParseLetDef(std::move(prop_ast));
    case Keyword::Def: return ParseFunDef(std::move(prop_ast));
    case Keyword::Declare: return ParseDeclare(std::move(prop_ast));
    default:;
  }
  // parse type, struct, enum and import
  if (IsTokenKeyword(Keyword::Type) || IsTokenKeyword(Keyword::Struct) ||
      IsTokenKeyword(Keyword::Enum) || IsTokenKeyword(Keyword::Import)) {
    if (prop == Prop::Extern || prop == Prop::Demangle) {
      logger().LogWarning("type aliases, structures, enumerates and "
                          "import cannot be 'extern', try using 'public'");
      prop_ast = MakeAST<PropertyAST>(Prop::Public);
    }
    switch (lexer()->key_val()) {
      case Keyword::Type: return ParseTypeAlias(std::move(prop_ast));
      case Keyword::Struct: return ParseStruct(std::move(prop_ast));
      case Keyword::Enum: return ParseEnum(std::move(prop_ast));
      case Keyword::Import: return ParseImport();
      default:;
    }
  }
  return nullptr;
}

bool Parser::ExpectChar(char c) {
  if (!IsTokenChar(c)) {
    std::string msg = "expected '";
    msg = msg + c + "'";
    LogError(msg.c_str());
    return false;
  }
  NextToken();
  return true;
}

bool Parser::ExpectId() {
  if (cur_token_ != Token::Id) {
    LogError("expected identifier");
    return false;
  }
  return true;
}

bool Parser::ExpectEOL() {
  if (last_token_ != Token::EOL) {
    LogError("expected line break or ';'");
    return false;
  }
  return true;
}
