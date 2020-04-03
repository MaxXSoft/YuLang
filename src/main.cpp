#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>

#include "version.h"

#include "define/type.h"
#include "front/logger.h"
#include "front/lexman.h"
#include "front/parser.h"
#include "front/analyzer.h"
#include "front/eval.h"
#include "mid/irbuilder.h"
#include "mid/passman.h"
#include "back/codegen.h"
#include "back/llvm/generator.h"

#include "xstl/argparse.h"

using namespace std;
using namespace yulang::define;
using namespace yulang::front;
using namespace yulang::mid;
using namespace yulang::back;
using namespace yulang::back::ll;

namespace {

xstl::ArgParser GetArgp() {
  xstl::ArgParser argp;
  argp.AddArgument<string>("input", "input source file");
  argp.AddOption<bool>("help", "h", "show this message", false);
  argp.AddOption<bool>("version", "v", "show version info", false);
  argp.AddOption<string>("outtype", "ot",
                         "type of output (ast/yuir/llvm/obj)", "llvm");
  argp.AddOption<string>("output", "o", "output file, default to stdout",
                         "");
  argp.AddOption<vector<string>>("imppath", "I", "add directory to "
                                 "import search path", {});
  argp.AddOption<int>("opt-level", "O", "set optimization level (0-3)", 0);
  argp.AddOption<bool>("verbose", "V", "use verbose output", false);
  argp.AddOption<bool>("warn-error", "Werror", "treat warnings as errors",
                       false);
  return argp;
}

void PrintVersion() {
  cout << APP_NAME << " version " << APP_VERSION << endl;
  cout << "Compiler of the Yu programming language." << endl;
  cout << endl;
  cout << "Copyright (C) 2010-2020 MaxXing. License GPLv3.";
  cout << endl;
}

void ParseArgument(xstl::ArgParser &argp, int argc, const char *argv[]) {
  auto ret = argp.Parse(argc, argv);
  // check if need to exit program
  if (argp.GetValue<bool>("help")) {
    argp.PrintHelp();
    std::exit(0);
  }
  else if (argp.GetValue<bool>("version")) {
    PrintVersion();
    std::exit(0);
  }
  else if (!ret) {
    cerr << "invalid input, run '";
    cerr << argp.program_name() << " -h' for help" << endl;
    std::exit(1);
  }
  // check output type
  auto out_type = argp.GetValue<string>("outtype");
  for (const auto &i : {"ast", "yuir", "llvm", "obj"}) {
    if (out_type == i) return;
  }
  Logger::LogRawError("invalid output type");
  std::exit(1);
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
  // check if need to exit
  auto err_num = Logger::error_num();
  if (err_num || dump_ast) std::exit(err_num);
}

void RunPasses(const xstl::ArgParser &argp, std::ostream &os,
               IRBuilder &irb) {
  // set optimization level
  PassManager pass_man;
  auto opt_level = argp.GetValue<int>("opt-level");
  if (opt_level < 0 || opt_level > 3) {
    Logger::LogRawError("invalid optimization level");
    std::exit(1);
  }
  pass_man.set_opt_level(opt_level);
  // run passes on IR
  if (argp.GetValue<bool>("verbose")) pass_man.ShowInfo(cerr);
  irb.module().RunPasses(pass_man);
  // check if need to dump IR
  auto out_type = argp.GetValue<string>("outtype");
  auto dump_yuir = out_type == "yuir";
  auto err_num = Logger::error_num();
  if (!err_num && dump_yuir) irb.module().Dump(os);
  if (err_num || dump_yuir) std::exit(err_num);
}

void GenerateCode(const xstl::ArgParser &argp, std::ostream &os,
                  IRBuilder &irb, CodeGen &gen) {
  // generate code
  irb.module().GenerateCode(gen);
  // check if need to dump code
  auto out_type = argp.GetValue<string>("outtype");
  auto dump_llvm = out_type == "llvm";
  if (dump_llvm) {
    gen.Dump(os);
    std::exit(0);
  }
}

}  // namespace

int main(int argc, const char *argv[]) {
  // set up argument parser
  auto argp = GetArgp();

  // parse argument
  ParseArgument(argp, argc, argv);

  // initialize output stream
  auto out_file = argp.GetValue<string>("output");
  std::ofstream ofs;
  if (!out_file.empty()) ofs.open(out_file);
  auto &os = out_file.empty() ? cout : ofs;

  // initialize compilation system
  LexerManager lex_man;
  IRBuilder irb;
  LLVMGen gen(argp.GetValue<string>("input"));
  // BaseType::set_ptr_size(gen.GetPointerSize());

  // compile source to target code
  CompileToIR(argp, os, lex_man, irb);
  RunPasses(argp, os, irb);
  GenerateCode(argp, os, irb, gen);
  return 0;
}
