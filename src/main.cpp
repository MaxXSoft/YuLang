#include <iostream>

#include "version.h"
#include "front/lexman.h"
#include "front/parser.h"
#include "front/analyzer.h"
#include "front/eval.h"

using namespace std;
using namespace yulang::front;

namespace {

//

}  // namespace

int main(int argc, const char *argv[]) {
  if (argc < 2) return 1;
  LexerManager lex_man;
  lex_man.LoadSource(argv[1]);
  Parser parser(lex_man);
  Evaluator eval;
  Analyzer ana(eval);
  while (auto ast = parser.ParseNext()) {
    if (!ast->SemaAnalyze(ana)) break;
    ast->Eval(eval);
    ast->Dump(cout);
  }
  return lex_man.lexer()->logger().error_num();
}
