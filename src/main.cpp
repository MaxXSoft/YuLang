#include <iostream>
#include <fstream>
#include <string>

#include "version.h"

#include "define/type.h"
#include "front/lexman.h"
#include "front/parser.h"
#include "front/analyzer.h"
#include "front/eval.h"
#include "mid/irbuilder.h"

#include "xstl/argparse.h"

using namespace std;
using namespace yulang::define;
using namespace yulang::front;
using namespace yulang::mid;

namespace {

void PrintVersion() {
  cout << APP_NAME << " version " << APP_VERSION << endl;
  cout << "Compiler of the Yu programming language." << endl;
  cout << endl;
  cout << "Copyright (C) 2010-2020 MaxXing. License GPLv3.";
  cout << endl;
}

}  // namespace

int main(int argc, const char *argv[]) {
  // set up argument parser
  xstl::ArgParser argp;
  argp.AddArgument<string>("input", "input source file");
  argp.AddOption<bool>("help", "h", "show this message", false);
  argp.AddOption<bool>("version", "v", "show version info", false);
  argp.AddOption<string>("outtype", "ot",
                         "type of output (ast/yuir/llvm/obj)", "yuir");
  argp.AddOption<string>("output", "o", "output file, default to stdout",
                         "");
  argp.AddOption<string>("imppath", "I", "set directory of import path",
                         "");

  // parse argument
  auto ret = argp.Parse(argc, argv);

  // check if need to exit program
  if (argp.GetValue<bool>("help")) {
    argp.PrintHelp();
    return 0;
  }
  else if (argp.GetValue<bool>("version")) {
    PrintVersion();
    return 0;
  }
  else if (!ret) {
    cerr << "invalid input, run '";
    cerr << argp.program_name() << " -h' for help" << endl;
    return 1;
  }

  // initialize lexer manager
  auto file = argp.GetValue<string>("input");
  auto imp_path = argp.GetValue<string>("imppath");
  LexerManager lex_man;
  lex_man.LoadSource(file);
  if (!imp_path.empty()) lex_man.AddImportPath(1, imp_path);

  // initialize other stuffs
  Parser parser(lex_man);
  Evaluator eval;
  Analyzer ana(eval);
  IRBuilder irb;

  // get output info
  auto out_type = argp.GetValue<string>("outtype");
  auto out_file = argp.GetValue<string>("output");
  auto dump_ast = out_type == "ast";
  auto dump_yuir = out_type == "yuir";

  // initialize output stream
  std::ofstream ofs;
  if (!out_file.empty()) ofs.open(out_file);
  auto &os = out_file.empty() ? cout : ofs;

  // compile source code
  while (auto ast = parser.ParseNext()) {
    // perform semantic analyze
    if (!ast->SemaAnalyze(ana)) break;
    ast->Eval(eval);
    // dump to output
    if (dump_ast) ast->Dump(os);
    // generate IR
    ast->GenerateIR(irb);
  }

  // check if is error
  auto err_num = lex_man.lexer()->logger().error_num();
  if (!err_num && dump_yuir) irb.module().Dump(os);
  return err_num;
}
