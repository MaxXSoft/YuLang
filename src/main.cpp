#include <iostream>
#include <fstream>
#include <string>
#include <vector>

#include "version.h"

#include "define/type.h"
#include "front/logger.h"
#include "front/lexman.h"
#include "front/parser.h"
#include "front/analyzer.h"
#include "front/eval.h"
#include "mid/irbuilder.h"
#include "mid/passman.h"

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

void CompileToIR(const xstl::ArgParser &argp, std::ostream &os,
                 LexerManager &lex_man, IRBuilder &irb) {
  // initialize lexer manager & logger
  auto file = argp.GetValue<string>("input");
  auto imp_path = argp.GetValue<vector<string>>("imppath");
  lex_man.LoadSource(file);
  if (!imp_path.empty()) {
    for (const auto &i : imp_path) lex_man.AddImportPath(1, i);
  }
  Logger::ResetErrorNum(argp.GetValue<bool>("warn-error"));
  // initialize other stuffs
  Parser parser(lex_man);
  Evaluator eval;
  Analyzer ana(eval);
  // get output info
  auto out_type = argp.GetValue<string>("outtype");
  auto dump_ast = out_type == "ast";
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
}

bool RunPasses(const xstl::ArgParser &argp, IRBuilder &irb) {
  // run passes on IR
  PassManager pass_man;
  auto opt_level = argp.GetValue<int>("opt-level");
  if (opt_level < 0 || opt_level > 3) {
    Logger::LogRawError("invalid optimization level");
    return false;
  }
  pass_man.set_opt_level(opt_level);
  if (argp.GetValue<bool>("verbose")) pass_man.ShowInfo(cerr);
  irb.module().RunPasses(pass_man);
  return true;
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
  argp.AddOption<vector<string>>("imppath", "I", "add directory to "
                                 "import search path", {});
  argp.AddOption<int>("opt-level", "O", "set optimization level (0-3)", 0);
  argp.AddOption<bool>("verbose", "V", "use verbose output", false);
  argp.AddOption<bool>("warn-error", "Werror", "treat warnings as errors",
                       false);

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

  // get output info
  auto out_type = argp.GetValue<string>("outtype");
  auto out_file = argp.GetValue<string>("output");
  auto dump_yuir = out_type == "yuir";

  // initialize output stream
  std::ofstream ofs;
  if (!out_file.empty()) ofs.open(out_file);
  auto &os = out_file.empty() ? cout : ofs;

  // compile source to IR
  LexerManager lex_man;
  IRBuilder irb;
  CompileToIR(argp, os, lex_man, irb);

  // check if is error after IR generation
  if (auto err_num = Logger::error_num()) return err_num;

  // run passes on IR
  RunPasses(argp, irb);

  // check if is error after passes running
  if (auto err_num = Logger::error_num()) return err_num;

  // dump IR
  if (dump_yuir) irb.module().Dump(os);
  return 0;
}
