#include "front/parser.h"

#include <sstream>
#include <stack>
#include <cassert>

#include "define/token.h"

using namespace yulang::front;
using namespace yulang::define;

namespace {

// type alias for unary operators
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

ASTPtr Parser::ParseVarLetDef(Property prop, bool is_var) {
  auto log = logger();
  // check property
  if (prop == Property::Extern) {
    log.LogWarning("var/let definitions cannot be 'extern', "
                   "try using 'public'");
    prop = Property::Public;
  }
  // go to next token
  NextToken();
  if (!ExpectId()) return nullptr;
  // get definition list
  ASTPtrList defs;
  for (;;) {
    auto elem = ParseVarLetElem(prop, is_var);
    if (!elem) return nullptr;
    defs.push_back(std::move(elem));
    // eat ','
    if (!IsTokenChar(',')) break;
    NextToken();
  }
  return MakeAST<VarLetDefAST>(log, prop, std::move(defs));
}

ASTPtr Parser::ParseFunDef(Property prop) {
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
  ASTPtr body;
  if (!in_import_ || prop == Property::Inline) {
    body = ParseBlock();
    if (!body) return nullptr;
  }
  return MakeAST<FunDefAST>(log, prop, name, std::move(args),
                            std::move(type), std::move(body));
}

ASTPtr Parser::ParseDeclare(Property prop) {
  auto log = logger();
  // check property
  if (prop == Property::Inline) {
    log.LogWarning("declarations cannot be 'inline', try using 'public'");
    prop = Property::Public;
  }
  NextToken();
  // check if is variable declaration
  bool is_var = false;
  if (IsTokenKeyword(Keyword::Var)) {
    is_var = true;
    NextToken();
  }
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // check & eat ':'
  if (!ExpectChar(':')) return nullptr;
  // get type
  auto type = ParseType();
  return MakeAST<DeclareAST>(log, prop, is_var, id, std::move(type));
}

ASTPtr Parser::ParseTypeAlias(Property prop) {
  auto log = logger();
  // check property
  if (prop == Property::Extern || prop == Property::Inline) {
    log.LogWarning("type aliases cannot be 'extern' or 'inline', "
                   "try using 'public'");
    prop = Property::Public;
  }
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // check & eat '='
  if (!IsTokenOperator(Operator::Assign)) return LogError("expected '='");
  NextToken();
  // get type
  auto type = ParseType();
  return MakeAST<TypeAliasAST>(log, prop, id, std::move(type));
}

ASTPtr Parser::ParseStruct(Property prop) {
  auto log = logger();
  // check property
  if (prop == Property::Extern || prop == Property::Inline) {
    log.LogWarning("structure definitions cannot be 'extern' "
                   "or 'inline', try using 'public'");
    prop = Property::Public;
  }
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
    auto elem = ParseStructElem();
    if (!elem) return nullptr;
    defs.push_back(std::move(elem));
    // eat ','
    if (!IsTokenChar(',')) break;
    NextToken();
  }
  // check & eat '}'
  if (!ExpectChar('}')) return nullptr;
  return MakeAST<StructAST>(log, prop, id, std::move(defs));
}

ASTPtr Parser::ParseEnum(Property prop) {
  auto log = logger();
  // check property
  if (prop == Property::Extern || prop == Property::Inline) {
    log.LogWarning("enumeration definitions cannot be 'extern' "
                   "or 'inline', try using 'public'");
    prop = Property::Public;
  }
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
  return MakeAST<EnumAST>(log, prop, id, std::move(type), std::move(defs));
}

ASTPtr Parser::ParseImport(Property prop) {
  auto log = logger();
  // check property
  if (prop == Property::Extern || prop == Property::Inline) {
    log.LogWarning("importations cannot be 'extern' "
                   "or 'inline', try using 'public'");
  }
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
  // get module path
  auto mod_path = lex_man_.GetModPath(mod_name);
  if (mod_path.empty()) return LogError("invalid module name");
  // check if module is loaded
  if (lex_man_.IsLoaded(mod_path)) {
    // return an empty 'ImportAST'
    return MakeAST<ImportAST>(log, ASTPtrList());
  }
  // switch lexer
  auto last_token = last_token_, cur_token = cur_token_;
  auto last_lex = lex_man_.SetLexer(mod_path);
  assert(last_lex && *last_lex);
  NextToken();
  // get all public/extern/inline definitions
  ASTPtrList defs;
  ++in_import_;
  while (!logger().error_num() && cur_token_ != Token::End) {
    auto prop = GetProp();
    if (prop != Property::None) {
      auto def = GetStatement(prop);
      if (!def) return nullptr;
      defs.push_back(std::move(def));
    }
    else {
      NextToken();
    }
  }
  --in_import_;
  // reset to original status
  lex_man_.SetLexer(*last_lex);
  last_token_ = last_token;
  cur_token_ = cur_token;
  return MakeAST<ImportAST>(log, std::move(defs));
}

ASTPtr Parser::ParseVarLetElem(Property prop, bool is_var) {
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
  if ((!in_import_ || prop == Property::None ||
       prop == Property::Inline) &&
      IsTokenOperator(Operator::Assign)) {
    NextToken();
    init = ParseExpr();
    if (!init) return nullptr;
  }
  return MakeAST<VarLetElemAST>(log, id, std::move(type), std::move(init),
                                is_var);
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

ASTPtr Parser::ParseStructElem() {
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
  return MakeAST<StructElemAST>(log, id, std::move(type));
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
  if (!IsTokenChar('}') && !ExpectEOL()) return nullptr;
  return stmt;
}

ASTPtr Parser::ParseBlockStatement() {
  // get normal statement
  if (!IsTokenKeyword(Keyword::Def)) {
    auto stmt = GetStatement(Property::None);
    if (stmt) return stmt;
  }
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
    NextToken();
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
  while (cur_token_ == Token::Operator && last_token_ != Token::EOL) {
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
  while (IsTokenKeyword(Keyword::As)) {
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
    NextToken();
    // get type
    auto type = ParseType();
    if (!type) return nullptr;
    return MakeAST<UnaryAST>(log, UnaryOp::SizeOf, std::move(type));
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
      default: factor = ParseValue(); break;
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
    else if (IsTokenOperator(Operator::Access)) {
      factor = ParseAccess(std::move(factor));
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
  if (!GetExprList(args)) return nullptr;
  return MakeAST<FunCallAST>(log, std::move(expr), std::move(args));
}

ASTPtr Parser::ParseAccess(ASTPtr expr) {
  auto log = logger();
  // eat '.'
  NextToken();
  // get id
  if (!ExpectId()) return nullptr;
  auto id = lexer()->id_val();
  NextToken();
  // check if is a dot function call
  if (IsTokenChar('(') && last_token_ != Token::EOL) {
    NextToken();
    // get argument list
    ASTPtrList args;
    args.push_back(std::move(expr));
    if (!GetExprList(args)) return nullptr;
    // generate function call
    auto id_ast = MakeAST<IdAST>(log, std::move(id));
    return MakeAST<FunCallAST>(log, std::move(id_ast), std::move(args));
  }
  // just access
  return MakeAST<AccessAST>(log, std::move(expr), std::move(id));
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
      if (IsTokenOperator(Operator::Mul) ||
          (cur_token_ == Token::Id && lexer()->id_val()[0] == '*')) {
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
  ASTPtr ast;
  if (IsTokenOperator(Operator::Mul)) {
    ast = MakeAST<PointerTypeAST>(is_var, std::move(type));
  }
  else {  // Token::Id
    for (const auto &c : lexer()->id_val()) {
      if (c != '*') LogError("expected '*'");
      if (!ast) {
        ast = MakeAST<PointerTypeAST>(is_var, std::move(type));
      }
      else {
        ast = MakeAST<PointerTypeAST>(false, std::move(ast));
      }
    }
  }
  NextToken();
  return ast;
}

ASTPtr Parser::ParseRef(bool is_var, ASTPtr type) {
  auto ast = MakeAST<RefTypeAST>(is_var, std::move(type));
  NextToken();
  return ast;
}

Property Parser::GetProp() {
  if (IsTokenKeyword(Keyword::Public)) {
    // eat 'public'
    NextToken();
    return Property::Public;
  }
  else if (IsTokenKeyword(Keyword::Extern)) {
    // eat 'extern'
    NextToken();
    return Property::Extern;
  }
  else if (IsTokenKeyword(Keyword::Inline)) {
    // eat 'inline'
    NextToken();
    return Property::Inline;
  }
  return Property::None;
}

ASTPtr Parser::GetStatement(Property prop) {
  if (cur_token_ != Token::Keyword) return nullptr;
  // parse statements by keyword
  switch (lexer()->key_val()) {
    case Keyword::Var: return ParseVarLetDef(prop, true);
    case Keyword::Let: return ParseVarLetDef(prop, false);
    case Keyword::Def: return ParseFunDef(prop);
    case Keyword::Declare: return ParseDeclare(prop);
    case Keyword::Type: return ParseTypeAlias(prop);
    case Keyword::Struct: return ParseStruct(prop);
    case Keyword::Enum: return ParseEnum(prop);
    case Keyword::Import: return ParseImport(prop);
    default: return nullptr;
  }
}

bool Parser::GetExprList(ASTPtrList &args) {
  // get expression list
  if (!IsTokenChar(')')) {
    for (;;) {
      auto arg = ParseExpr();
      if (!arg) return false;
      args.push_back(std::move(arg));
      // eat ','
      if (!IsTokenChar(',')) break;
      NextToken();
    }
  }
  // check & eat ')'
  return ExpectChar(')');
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
  if (last_token_ != Token::EOL && cur_token_ != Token::End) {
    LogError("expected line break or ';'");
    return false;
  }
  return true;
}
