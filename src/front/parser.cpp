#include "front/parser.h"

#include <array>
#include <cassert>
#include <sstream>
#include <stack>

#include "define/token.h"
#include "xstl/guard.h"

namespace yulang::front {

using yulang::define::AccessAST;
using yulang::define::ArgElemAST;
using yulang::define::ArrayTypeAST;
using yulang::define::AsmAST;
using yulang::define::ASTPtr;
using yulang::define::ASTPtrList;
using yulang::define::BinaryAST;
using yulang::define::BlockAST;
using yulang::define::BoolAST;
using yulang::define::CastAST;
using yulang::define::CharAST;
using yulang::define::ControlAST;
using yulang::define::DeclareAST;
using yulang::define::EnumAST;
using yulang::define::EnumElemAST;
using yulang::define::FloatAST;
using yulang::define::ForInAST;
using yulang::define::FunCallAST;
using yulang::define::FuncTypeAST;
using yulang::define::FunDefAST;
using yulang::define::IdAST;
using yulang::define::IfAST;
using yulang::define::ImportAST;
using yulang::define::ImportBinding;
using yulang::define::ImportKind;
using yulang::define::IndexAST;
using yulang::define::IntAST;
using yulang::define::IsExtern;
using yulang::define::IsInline;
using yulang::define::Keyword;
using yulang::define::NullAST;
using yulang::define::Operator;
using yulang::define::PointerTypeAST;
using yulang::define::PrimTypeAST;
using yulang::define::Property;
using yulang::define::RefTypeAST;
using yulang::define::StringAST;
using yulang::define::StructAST;
using yulang::define::StructElemAST;
using yulang::define::Token;
using yulang::define::TypeAliasAST;
using yulang::define::UnaryAST;
using yulang::define::UserTypeAST;
using yulang::define::ValInitAST;
using yulang::define::VarLetDefAST;
using yulang::define::VarLetElemAST;
using yulang::define::VolaTypeAST;
using yulang::define::WhenAST;
using yulang::define::WhenElemAST;
using yulang::define::WhileAST;

namespace {

// type alias for unary operators
using UnaryOp = UnaryAST::UnaryOp;

// table of operator's precedence
// -1 if it's not a binary/assign operator
const std::array kOpPrecTable = {YULANG_OPERATORS(YULANG_EXPAND_THIRD)};

// table of operator's name
const std::array kOperators = {YULANG_OPERATORS(YULANG_EXPAND_SECOND)};

// return precedence of specific operator
inline int GetOpPrec(Operator op) { return kOpPrecTable[static_cast<int>(op)]; }

}  // namespace

ASTPtr Parser::LogError(std::string_view message) {
  // The lexer already diagnosed this token.
  if (cur_token_ != Token::Error) logger().LogError(message);
  return nullptr;
}

ASTPtr Parser::ParseNext() {
  if (cur_token_ == Token::End) {
    ended_ = true;
    return nullptr;
  }
  const auto start = token_index_;
  auto stmt = ParseLine();
  if (!stmt) Synchronize(0, start, false);
  return stmt;
}

define::SourceModulePtr Parser::BeginModule(const std::filesystem::path &path) {
  auto module = std::make_shared<define::SourceModule>();
  module->path = path.string();
  module->id = lex_man_.ModuleId(path);
  module->anonymous = module->id.empty();
  if (module->anonymous) module->id = "<anonymous>";
  module_cache_.emplace(module->path, module);
  modules_.push_back(module);
  return module;
}

void Parser::ParseModuleContents(const define::SourceModulePtr &module) {
  loading_.insert(module->path);
  import_stack_.push_back(module->id);
  const xstl::Guard loading([&] {
    loading_.erase(module->path);
    import_stack_.pop_back();
  });
  while (!ended_) {
    if (auto ast = ParseNext()) module->defs.push_back(std::move(ast));
  }
}

define::SourceModulePtr Parser::ParseModule() {
  if (root_) return root_;
  root_ = BeginModule(std::filesystem::path(logger().cur_file()));
  current_module_id_ = root_->id;
  ParseModuleContents(root_);
  return root_;
}

define::SourceModulePtr Parser::ParseImportedModule(
    const std::filesystem::path &path) {
  const auto key = path.string();
  if (loading_.find(key) != loading_.end()) {
    std::string chain;
    for (const auto &id : import_stack_) {
      if (!chain.empty()) chain += " -> ";
      chain += id;
    }
    chain += " -> " + lex_man_.ModuleId(path);
    logger().LogError("cyclic import: " + chain);
    return {};
  }
  if (const auto found = module_cache_.find(key);
      found != module_cache_.end()) {
    return found->second;
  }
  const auto previous_last = last_token_;
  const auto previous_current = cur_token_;
  const auto previous_ended = ended_;
  const auto previous_depth = brace_depth_;
  const auto previous_index = token_index_;
  const auto previous_module = current_module_id_;
  const auto previous_lexer = lex_man_.SetLexer(path);
  if (!previous_lexer || !*previous_lexer) {
    logger().LogError("failed to load imported module");
    return {};
  }
  const xstl::Guard restore([&, this] {
    lex_man_.SetLexer(*previous_lexer);
    last_token_ = previous_last;
    cur_token_ = previous_current;
    ended_ = previous_ended;
    brace_depth_ = previous_depth;
    token_index_ = previous_index;
    current_module_id_ = previous_module;
  });
  auto module = BeginModule(path);
  current_module_id_ = module->id;
  last_token_ = cur_token_ = Token::End;
  ended_ = false;
  brace_depth_ = token_index_ = 0;
  NextToken();
  ParseModuleContents(module);
  return module;
}

void Parser::Synchronize(std::size_t depth, std::size_t start, bool in_block) {
  while (cur_token_ != Token::End) {
    if (brace_depth_ <= depth) {
      if (cur_token_ == Token::EOL) {
        NextToken();
        return;
      }
      if (token_index_ != start && last_token_ == Token::EOL) return;
      if (IsTokenChar('}')) {
        if (!in_block) NextToken();
        return;
      }
    }
    NextTokenKeepEOL();
  }
}

ASTPtr Parser::ParseLine() {
  const auto errors = Logger::error_num();
  auto stmt = GetStatement(GetProp());
  if (!stmt) {
    if (Logger::error_num() == errors) LogError("invalid statement");
    return nullptr;
  }
  if (!ExpectEOL()) return nullptr;
  return stmt;
}

ASTPtr Parser::ParseVarLetDef(Property prop, bool is_var) {
  auto log = logger();
  // check property
  if (IsExtern(prop)) return LogError("var/let definitions cannot be 'extern'");
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
  } else if (cur_token_ == Token::Operator) {
    name = kOperators[static_cast<int>(lexer()->op_val())];
  } else {
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
      if (IsTokenChar(')')) break;
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
  return MakeAST<FunDefAST>(log, prop, name, std::move(args), std::move(type),
                            std::move(body));
}

ASTPtr Parser::ParseDeclare(Property prop) {
  auto log = logger();
  // check property
  if (IsInline(prop)) return LogError("declarations cannot be 'inline'");
  NextToken();
  // check if is variable declaration
  bool is_var = false;
  if (IsTokenKeyword(Keyword::Var)) {
    is_var = true;
    NextToken();
  }
  // get identifier
  if (!ExpectId()) return nullptr;
  const auto id = lexer()->id_val();
  NextToken();
  // check & eat ':'
  if (!ExpectChar(':')) return nullptr;
  // get type
  auto type = ParseType();
  if (!type) return nullptr;
  return MakeAST<DeclareAST>(log, prop, is_var, id, std::move(type));
}

ASTPtr Parser::ParseTypeAlias(Property prop) {
  auto log = logger();
  // check property
  if (IsExtern(prop) || IsInline(prop)) {
    return LogError("type aliases cannot be 'extern' or 'inline'");
  }
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  const auto id = lexer()->id_val();
  NextToken();
  // check & eat '='
  if (!IsTokenOperator(Operator::Assign)) return LogError("expected '='");
  NextToken();
  // get type
  auto type = ParseType();
  if (!type) return nullptr;
  return MakeAST<TypeAliasAST>(log, prop, id, std::move(type));
}

ASTPtr Parser::ParseStruct(Property prop) {
  auto log = logger();
  // check property
  if (IsExtern(prop) || IsInline(prop)) {
    return LogError("structure definitions cannot be 'extern' or 'inline'");
  }
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  const auto id = lexer()->id_val();
  NextToken();
  // check & eat '{'
  if (!ExpectChar('{')) return nullptr;
  // get definitions of all fields
  ASTPtrList defs;
  while (cur_token_ == Token::Id || IsTokenKeyword(Keyword::Public)) {
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
  if (IsExtern(prop) || IsInline(prop)) {
    return LogError("enumeration definitions cannot be 'extern' or 'inline'");
  }
  NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  const auto id = lexer()->id_val();
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
  if (prop != Property::None) {
    return LogError("import bindings cannot be public, extern, or inline");
  }
  NextToken();
  ModName path;
  bool grouped = false;
  bool wildcard = false;
  for (;;) {
    if (!ExpectId()) return nullptr;
    path.push_back(lexer()->id_val());
    NextToken();
    // The lexer preserves operator-formed identifiers, so adjacent punctuation
    // such as '.*' and '.==' arrives as one identifier token.
    if (cur_token_ == Token::Id && lexer()->id_val().size() > 1 &&
        lexer()->id_val().front() == '.') {
      auto name = lexer()->id_val().substr(1);
      NextToken();
      if (name == "*") {
        wildcard = true;
      } else {
        path.push_back(std::move(name));
      }
      break;
    }
    if (!IsTokenOperator(Operator::Access)) break;
    NextToken();
    if (IsTokenChar('{')) {
      grouped = true;
      break;
    }
    if (IsTokenOperator(Operator::Mul)) {
      wildcard = true;
      NextToken();
      break;
    }
    // Operator overloads may be selected by name as well.
    if (cur_token_ == Token::Operator) {
      path.emplace_back(kOperators[static_cast<int>(lexer()->op_val())]);
      NextToken();
      break;
    }
  }
  if (path.size() < 2) {
    return LogError("import requires a library-qualified source module");
  }

  std::vector<ImportBinding> bindings;
  if (grouped) {
    NextToken();
    if (IsTokenChar('}')) return LogError("expected imported definition");
    for (;;) {
      std::string name;
      if (cur_token_ == Token::Id) {
        name = lexer()->id_val();
      } else if (cur_token_ == Token::Operator &&
                 !IsTokenOperator(Operator::Access)) {
        name = kOperators[static_cast<int>(lexer()->op_val())];
      } else {
        return LogError("expected imported definition");
      }
      NextToken();
      auto alias = name;
      if (IsTokenKeyword(Keyword::As)) {
        NextToken();
        if (!ExpectId()) return nullptr;
        alias = lexer()->id_val();
        NextToken();
      }
      bindings.push_back({std::move(name), std::move(alias), {}});
      if (!IsTokenChar(',')) break;
      NextToken();
      if (IsTokenChar('}')) break;
    }
    if (!ExpectChar('}')) return nullptr;
  } else if (!wildcard) {
    auto alias = path.back();
    if (IsTokenKeyword(Keyword::As)) {
      NextToken();
      if (!ExpectId()) return nullptr;
      alias = lexer()->id_val();
      NextToken();
    }
    bindings.push_back({path.back(), std::move(alias), {}});
  }

  const auto full_path = lex_man_.GetModPath(path);
  if (grouped || wildcard) {
    if (full_path.empty())
      return LogError("import target is not a source module");
    auto module = ParseImportedModule(full_path);
    if (!module) return nullptr;
    for (auto &binding : bindings) {
      auto alternative = path;
      alternative.push_back(binding.name);
      const auto alternative_path = lex_man_.GetModPath(alternative);
      if (!alternative_path.empty()) {
        binding.module_alternative = ParseImportedModule(alternative_path);
        if (!binding.module_alternative) return nullptr;
      }
    }
    return MakeAST<ImportAST>(
        log, std::move(module),
        wildcard ? ImportKind::Wildcard : ImportKind::Selected,
        std::move(bindings));
  }

  auto prefix = path;
  const auto member = prefix.back();
  prefix.pop_back();
  const auto prefix_path = lex_man_.GetModPath(prefix);
  if (full_path.empty() && prefix_path.empty()) {
    return LogError(
        "import target is not a source module or module definition");
  }
  if (!full_path.empty()) {
    auto module = ParseImportedModule(full_path);
    if (!module) return nullptr;
    define::SourceModulePtr alternative;
    if (!prefix_path.empty()) {
      alternative = ParseImportedModule(prefix_path);
      if (!alternative) return nullptr;
    }
    return MakeAST<ImportAST>(log, std::move(module), ImportKind::Namespace,
                              std::move(bindings), std::move(alternative),
                              member);
  }
  auto module = ParseImportedModule(prefix_path);
  if (!module) return nullptr;
  return MakeAST<ImportAST>(log, std::move(module), ImportKind::Selected,
                            std::move(bindings));
}

ASTPtr Parser::ParseVarLetElem(Property /*prop*/, bool is_var) {
  auto log = logger();
  // get identifier
  if (!ExpectId()) return nullptr;
  const auto id = lexer()->id_val();
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
  return MakeAST<VarLetElemAST>(log, id, std::move(type), std::move(init),
                                is_var);
}

ASTPtr Parser::ParseArgElem() {
  auto log = logger();
  // get identifier
  if (!ExpectId()) return nullptr;
  const auto id = lexer()->id_val();
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
  const bool is_public = IsTokenKeyword(Keyword::Public);
  if (is_public) NextToken();
  // get identifier
  if (!ExpectId()) return nullptr;
  const auto id = lexer()->id_val();
  NextToken();
  // check ':'
  if (!ExpectChar(':')) return nullptr;
  // get type
  auto type = ParseType();
  if (!type) return nullptr;
  return MakeAST<StructElemAST>(log, id, std::move(type), is_public);
}

ASTPtr Parser::ParseEnumElem() {
  auto log = logger();
  // get identifier
  const auto id = lexer()->id_val();
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
  const auto depth = brace_depth_;
  bool valid = true;
  while (!IsTokenChar('}') && cur_token_ != Token::End) {
    const auto start = token_index_;
    auto stmt = ParseBlockLine();
    if (stmt) {
      stmts.push_back(std::move(stmt));
    } else {
      valid = false;
      Synchronize(depth, start, true);
    }
  }
  // eat '}'
  if (!ExpectChar('}') || !valid) return nullptr;
  return MakeAST<BlockAST>(log, std::move(stmts));
}

ASTPtr Parser::ParseBlockLine() {
  auto stmt = ParseBlockStatement();
  if (!stmt) return nullptr;
  if (!IsTokenChar('}') && !ExpectEOL()) return nullptr;
  return stmt;
}

ASTPtr Parser::ParseBlockStatement() {
  // check if is invalid statement
  if (IsTokenKeyword(Keyword::Def) || IsTokenKeyword(Keyword::Import)) {
    return LogError("invalid in-block statement");
  }
  // get normal statement
  const auto errors = Logger::error_num();
  auto stmt = GetStatement(Property::None);
  if (stmt) return stmt;
  if (Logger::error_num() != errors || cur_token_ == Token::Error)
    return nullptr;
  // parse other in-block statements
  if (cur_token_ == Token::Keyword) {
    switch (lexer()->key_val()) {
      case Keyword::While:
        return ParseWhile();
      case Keyword::For:
        return ParseForIn();
      case Keyword::Asm:
        return ParseAsm();
      case Keyword::Break:
      case Keyword::Continue:
      case Keyword::Return:
        return ParseControl();
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
  const auto id = lexer()->id_val();
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
    oss << lexer()->str_val() << '\n';
    NextToken();
  }
  // check & eat '}'
  if (!ExpectChar('}')) return nullptr;
  return MakeAST<AsmAST>(log, oss.str());
}

ASTPtr Parser::ParseControl() {
  auto log = logger();
  // get keyword type
  const auto type = lexer()->key_val();
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
    lhs = MakeAST<FunCallAST>(log, std::move(id), std::move(args),
                              FunCallAST::CallKind::Infix);
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
    const auto op = lexer()->op_val();
    if (GetOpPrec(op) < 0) break;
    NextToken();
    // handle operator
    while (!ops.empty() && GetOpPrec(ops.top()) >= GetOpPrec(op)) {
      // create a new binary AST
      const auto cur_op = ops.top();
      ops.pop();
      auto rhs = std::move(oprs.top());
      oprs.pop();
      auto lhs = std::move(oprs.top());
      oprs.pop();
      oprs.push(
          MakeAST<BinaryAST>(log, cur_op, std::move(lhs), std::move(rhs)));
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
    const auto cur_op = ops.top();
    ops.pop();
    auto rhs = std::move(oprs.top());
    oprs.pop();
    auto lhs = std::move(oprs.top());
    oprs.pop();
    oprs.push(MakeAST<BinaryAST>(log, cur_op, std::move(lhs), std::move(rhs)));
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
  UnaryOp una_op{};
  // check if need to get operator
  if (cur_token_ == Token::Operator) {
    // get & check unary operator
    const auto op = lexer()->op_val();
    switch (op) {
      case Operator::Add:
        una_op = UnaryOp::Pos;
        break;
      case Operator::Sub:
        una_op = UnaryOp::Neg;
        break;
      case Operator::LogicNot:
        una_op = UnaryOp::LogicNot;
        break;
      case Operator::Not:
        una_op = UnaryOp::Not;
        break;
      case Operator::Mul:
        una_op = UnaryOp::DeRef;
        break;
      case Operator::And:
        una_op = UnaryOp::AddrOf;
        break;
      default:
        return LogError("invalid unary operator");
    }
  } else if (IsTokenKeyword(Keyword::SizeOf)) {
    NextToken();
    // get type
    auto type = ParseType();
    if (!type) return nullptr;
    return MakeAST<UnaryAST>(log, UnaryOp::SizeOf, std::move(type));
  } else {
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
      case Keyword::If:
        factor = ParseIfElse();
        break;
      case Keyword::When:
        factor = ParseWhen();
        break;
      default:
        factor = ParseValue();
        break;
    }
  } else if (IsTokenChar('{')) {
    // block
    factor = ParseBlock();
  } else if (IsTokenChar('(')) {
    // bracket expression
    NextToken();
    factor = ParseExpr();
    if (!factor) return nullptr;
    if (!ExpectChar(')')) return nullptr;
  } else {
    // other values
    factor = ParseValue();
  }
  // parse the rest part
  while (factor && last_token_ != Token::EOL) {
    if (IsTokenChar('[')) {
      factor = ParseIndex(std::move(factor));
    } else if (IsTokenChar('(')) {
      factor = ParseFunCall(std::move(factor));
    } else if (IsTokenOperator(Operator::Access)) {
      factor = ParseAccess(std::move(factor));
    } else {
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
  const auto id = lexer()->id_val();
  NextToken();
  auto access = MakeAST<AccessAST>(log, std::move(expr), id);
  // check if is a dot function call
  if (IsTokenChar('(') && last_token_ != Token::EOL) {
    NextToken();
    // get argument list
    ASTPtrList args;
    if (!GetExprList(args)) return nullptr;
    return MakeAST<FunCallAST>(log, std::move(access), std::move(args),
                               FunCallAST::CallKind::Dot);
  }
  // just access
  return access;
}

ASTPtr Parser::ParseValue() {
  switch (cur_token_) {
    case Token::Int:
      return ParseInt();
    case Token::Float:
      return ParseFloat();
    case Token::Char:
      return ParseChar();
    case Token::Id:
      return ParseId();
    case Token::String:
      return ParseString();
    case Token::Keyword: {
      switch (lexer()->key_val()) {
        case Keyword::True:
        case Keyword::False:
          return ParseBool();
        case Keyword::Null:
          return ParseNull();
        default:
          return LogError("invalid value");
      }
    }
    default: {
      if (IsTokenChar('[')) {
        return ParseValInit();
      }
      return LogError("invalid value");
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
  // An empty pair of brackets takes its type from the expression context.
  ASTPtr type;
  if (!IsTokenChar(']')) {
    type = ParseType();
    if (!type) return nullptr;
  }
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
    } else {
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
      } else if (IsTokenOperator(Operator::And)) {
        // reference type
        type = ParseRef(is_var, std::move(type));
      } else {
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
  if (cur_token_ == Token::Keyword) {
    // primitive type
    return ParsePrimType();
  }
  if (cur_token_ == Token::Id) {
    // user defined type
    auto log = logger();
    std::string name = lexer()->id_val();
    NextToken();
    while (last_token_ != Token::EOL && IsTokenOperator(Operator::Access)) {
      NextToken();
      if (!ExpectId()) return nullptr;
      name += '.';
      name += lexer()->id_val();
      NextToken();
    }
    return MakeAST<UserTypeAST>(log, std::move(name));
  }
  return LogError("invalid value type");
}

ASTPtr Parser::ParsePrimType() {
  switch (lexer()->key_val()) {
    case Keyword::Int8:
    case Keyword::Int16:
    case Keyword::Int32:
    case Keyword::Int64:
    case Keyword::ISize:
    case Keyword::UInt8:
    case Keyword::UInt16:
    case Keyword::UInt32:
    case Keyword::UInt64:
    case Keyword::USize:
    case Keyword::Float32:
    case Keyword::Float64:
    case Keyword::Bool:
      break;
    default:
      return LogError("expected primitive type keywords");
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
      if (IsTokenChar(')')) break;
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
  } else {  // Token::Id
    for (const auto &c : lexer()->id_val()) {
      if (c != '*') return LogError("expected '*'");
      if (!ast) {
        ast = MakeAST<PointerTypeAST>(is_var, std::move(type));
      } else {
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
  auto prop = Property::None;
  if (IsTokenKeyword(Keyword::Public)) {
    // eat 'public'
    NextToken();
    prop = Property::Public;
  }
  if (IsTokenKeyword(Keyword::Extern)) {
    // eat 'extern'
    NextToken();
    prop = prop | Property::Extern;
  } else if (IsTokenKeyword(Keyword::Inline)) {
    // eat 'inline'
    NextToken();
    prop = prop | Property::Inline;
  }
  return prop;
}

ASTPtr Parser::GetStatement(Property prop) {
  if (cur_token_ != Token::Keyword) return nullptr;
  // parse statements by keyword
  switch (lexer()->key_val()) {
    case Keyword::Var:
      return ParseVarLetDef(prop, true);
    case Keyword::Let:
      return ParseVarLetDef(prop, false);
    case Keyword::Def:
      return ParseFunDef(prop);
    case Keyword::Declare:
      return ParseDeclare(prop);
    case Keyword::Type:
      return ParseTypeAlias(prop);
    case Keyword::Struct:
      return ParseStruct(prop);
    case Keyword::Enum:
      return ParseEnum(prop);
    case Keyword::Import:
      return ParseImport(prop);
    default:
      return nullptr;
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
      if (IsTokenChar(')')) break;
    }
  }
  // check & eat ')'
  return ExpectChar(')');
}

bool Parser::ExpectChar(char c) {
  if (!IsTokenChar(c)) {
    std::string msg = "expected '";
    msg = msg + c + "'";
    LogError(msg);
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

}  // namespace yulang::front
