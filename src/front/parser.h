#ifndef YULANG_FRONT_PARSER_H_
#define YULANG_FRONT_PARSER_H_

#include <string_view>

#include "define/token.h"
#include "define/ast.h"
#include "front/lexman.h"

namespace yulang::front {

class Parser {
 public:
  Parser(LexerManager &lex_man) : lex_man_(lex_man) { Reset(); }

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
    else {
      return ParseLine();
    }
  }

  // getters
  // returns true if parser met EOF
  bool ended() const { return ended_; }

 private:
  // get next token from lexer and skip all EOLs
  define::Token NextToken() {
    while (NextTokenKeepEOL() == define::Token::EOL);
    return cur_token_;
  }
  // get next token from lexer without skipping EOLs
  define::Token NextTokenKeepEOL() {
    last_token_ = cur_token_;
    return cur_token_ = logger().error_num() ? define::Token::Error
                                             : lexer()->NextToken();
  }

  // check if current token is a character (token type 'Other')
  bool IsTokenChar(char c) const {
    using namespace define;
    return (cur_token_ == Token::Other && lexer()->other_val() == c) ||
           (cur_token_ == Token::Id && lexer()->id_val().size() == 1 &&
            lexer()->id_val()[0] == c);
  }

  // check if current token is a keyword
  bool IsTokenKeyword(define::Keyword key) const {
    using namespace define;
    return cur_token_ == Token::Keyword && lexer()->key_val() == key;
  }

  // check if current token is an operator
  bool IsTokenOperator(define::Operator op) const {
    using namespace define;
    return cur_token_ == Token::Operator && lexer()->op_val() == op;
  }

  // check if current token is an assignment operator
  bool IsAssign() const {
    using namespace define;
    return cur_token_ == Token::Operator &&
           IsOperatorAssign(lexer()->op_val());
  }

  // create a new AST
  template <typename T, typename... Args>
  define::ASTPtr MakeAST(Args &&... args) {
    auto ast = std::make_unique<T>(std::forward<Args>(args)...);
    ast->set_logger(logger());
    return ast;
  }

  // create a new AST with specific logger
  template <typename T, typename... Args>
  define::ASTPtr MakeAST(Logger &logger, Args &&... args) {
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
  const LexerPtr &lexer() const { return lex_man_.lexer(); }
  // current logger
  const Logger &logger() const { return lex_man_.lexer()->logger(); }

  LexerManager &lex_man_;
  define::Token last_token_, cur_token_;
  bool ended_;
  unsigned int in_import_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_PARSER_H_
