#ifndef YULANG_FRONT_PARSER_H_
#define YULANG_FRONT_PARSER_H_

#include <string_view>

#include "define/ast.h"
#include "define/token.h"
#include "front/lexman.h"

namespace yulang::front {

class Parser {
 public:
  explicit Parser(LexerManager &lex_man) : lex_man_(lex_man) { Reset(); }

  // reset parser status
  void Reset() {
    lexer()->Reset();
    ended_ = false;
    in_import_ = 0;
    NextToken();
  }

  // get next AST from token stream
  define::ASTPtr ParseNext() {
    if (cur_token_ == define::Token::End) {
      ended_ = true;
      return nullptr;
    }
    return ParseLine();
  }

  // getters
  // returns true if parser met EOF
  [[nodiscard]] bool ended() const { return ended_; }

 private:
  // get next token from lexer and skip all EOLs
  define::Token NextToken() {
    while (NextTokenKeepEOL() == define::Token::EOL);
    return cur_token_;
  }
  // get next token from lexer without skipping EOLs
  define::Token NextTokenKeepEOL() {
    last_token_ = cur_token_;
    return cur_token_ = Logger::error_num() ? define::Token::Error
                                            : lexer()->NextToken();
  }

  // check if current token is a character (token type 'Other')
  [[nodiscard]] bool IsTokenChar(char c) const {
    using define::Token;
    return (cur_token_ == Token::Other && lexer()->other_val() == c) ||
           (cur_token_ == Token::Id && lexer()->id_val().size() == 1 &&
            lexer()->id_val()[0] == c);
  }

  // check if current token is a keyword
  [[nodiscard]] bool IsTokenKeyword(define::Keyword key) const {
    using define::Token;
    return cur_token_ == Token::Keyword && lexer()->key_val() == key;
  }

  // check if current token is an operator
  [[nodiscard]] bool IsTokenOperator(define::Operator op) const {
    using define::Token;
    return cur_token_ == Token::Operator && lexer()->op_val() == op;
  }

  // check if current token is an assignment operator
  [[nodiscard]] bool IsAssign() const {
    using define::Token;
    return cur_token_ == Token::Operator &&
           define::IsOperatorAssign(lexer()->op_val());
  }

  // create a new AST
  template <typename T, typename... Args>
  define::ASTPtr MakeAST(Args &&...args) {
    auto ast = std::make_unique<T>(std::forward<Args>(args)...);
    ast->set_logger(logger());
    return ast;
  }

  // create a new AST with specific logger
  template <typename T, typename... Args>
  define::ASTPtr MakeAST(Logger &logger, Args &&...args) {
    auto ast = std::make_unique<T>(std::forward<Args>(args)...);
    ast->set_logger(logger);
    return ast;
  }

  // log error and return null pointer
  define::ASTPtr LogError(std::string_view message);

  define::ASTPtr ParseLine();

  define::ASTPtr ParseVarLetDef(define::Property prop, bool is_var);
  define::ASTPtr ParseFunDef(define::Property prop);
  define::ASTPtr ParseDeclare(define::Property prop);
  define::ASTPtr ParseTypeAlias(define::Property prop);
  define::ASTPtr ParseStruct(define::Property prop);
  define::ASTPtr ParseEnum(define::Property prop);
  define::ASTPtr ParseImport(define::Property prop);

  define::ASTPtr ParseVarLetElem(define::Property prop, bool is_var);
  define::ASTPtr ParseArgElem();
  define::ASTPtr ParseStructElem();
  define::ASTPtr ParseEnumElem();

  define::ASTPtr ParseBlock();
  define::ASTPtr ParseBlockLine();
  define::ASTPtr ParseBlockStatement();

  define::ASTPtr ParseIfElse();
  define::ASTPtr ParseWhen();
  define::ASTPtr ParseWhile();
  define::ASTPtr ParseForIn();
  define::ASTPtr ParseAsm();
  define::ASTPtr ParseControl();

  define::ASTPtr ParseWhenElem();

  define::ASTPtr ParseExpr();
  define::ASTPtr ParseBinary();
  define::ASTPtr ParseCast();
  define::ASTPtr ParseUnary();
  define::ASTPtr ParseFactor();

  define::ASTPtr ParseIndex(define::ASTPtr expr);
  define::ASTPtr ParseFunCall(define::ASTPtr expr);
  define::ASTPtr ParseAccess(define::ASTPtr expr);

  define::ASTPtr ParseValue();
  define::ASTPtr ParseInt();
  define::ASTPtr ParseFloat();
  define::ASTPtr ParseChar();
  define::ASTPtr ParseId();
  define::ASTPtr ParseString();
  define::ASTPtr ParseBool();
  define::ASTPtr ParseNull();
  define::ASTPtr ParseValInit();

  define::ASTPtr ParseType();
  define::ASTPtr ParseValType();
  define::ASTPtr ParsePrimType();
  define::ASTPtr ParseFunc();
  define::ASTPtr ParseVolaType(define::ASTPtr type);
  define::ASTPtr ParseArray(define::ASTPtr type);
  define::ASTPtr ParsePointer(bool is_var, define::ASTPtr type);
  define::ASTPtr ParseRef(bool is_var, define::ASTPtr type);

  // try to get property and goto next token
  define::Property GetProp();
  // parse statement, returns nullptr if failed
  define::ASTPtr GetStatement(define::Property prop);
  // parse expression list, returns false if failed
  bool GetExprList(define::ASTPtrList &args);
  // make sure current token is specific character and goto next token
  bool ExpectChar(char c);
  // make sure current token is identifier
  bool ExpectId();
  // make sure last token is end of line
  bool ExpectEOL();

  // private getters
  // current lexer
  [[nodiscard]] const LexerPtr &lexer() const { return lex_man_.lexer(); }
  // current logger
  [[nodiscard]] const Logger &logger() const {
    return lex_man_.lexer()->logger();
  }

  // This service is bound to its owner for its entire lifetime.
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  LexerManager &lex_man_;
  define::Token last_token_{define::Token::End}, cur_token_{define::Token::End};
  bool ended_{};
  unsigned int in_import_{};
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_PARSER_H_
