#include <cstdint>
#include <cstdlib>
#include <exception>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "back/codegen.h"
#include "back/llvm/generator.h"
#include "back/llvm/objgen.h"
#include "define/panic.h"
#include "define/type.h"
#include "front/analyzer.h"
#include "front/depfile.h"
#include "front/eval.h"
#include "front/lexman.h"
#include "front/logger.h"
#include "front/parser.h"
#include "mid/irbuilder.h"
#include "mid/passman.h"
#include "version.h"
#include "xstl/argparse.h"

using std::cerr;
using std::cout;
using std::string;
using std::vector;
using yulang::define::BaseType;
using yulang::front::Analyzer;
using yulang::front::Evaluator;
using yulang::front::LexerManager;
using yulang::front::Logger;
using yulang::front::Parser;
using yulang::mid::IRBuilder;
using yulang::mid::PassManager;

using yulang::back::ll::LLVMGen;
using yulang::back::ll::ObjectGen;

using yulang::back::CodeGen;

namespace {

enum class OutputType : std::uint8_t {
  AST,
  YuIR,
  LLVM,
  Assembly,
  Object,
};

xstl::ArgParser GetArgp() {
  xstl::ArgParser argp;
  argp.AddArgument<string>("input", "input source file");
  argp.AddOption<bool>("help", "h", "show this message", false);
  argp.AddOption<bool>("version", "v", "show version info", false);
  argp.AddOption<string>("out-type", "ot",
                         "type of output (ast/yuir/llvm/asm/obj)", "obj");
  argp.AddOption<string>("output", "o", "output file, default to stdout", "");
  argp.AddOption<bool>("deps", "MD",
                       "write dependencies to <output>.d (requires -o)", false);
  argp.AddOption<vector<string>>("import-path", "I",
                                 "add directory to import search path", {});
  argp.AddOption<vector<string>>("define", "D",
                                 "define a single-level macro (NAME=TEXT)", {});
  argp.AddOption<int>("opt-level", "O", "set optimization level (0-3)", 0);
  argp.AddOption<bool>("verbose", "V", "use verbose output", false);
  argp.AddOption<bool>("warn-error", "Werror", "treat warnings as errors",
                       false);
  argp.AddOption<string>("target", "tt", "specify target triple", "");
  argp.AddOption<string>("cpu", "tc", "specify target CPU", "generic");
  argp.AddOption<string>("features", "tf", "specify target features", "");
  return argp;
}

void PrintVersion() {
  cout << APP_NAME << " version " << APP_VERSION << '\n';
  cout << "Compiler of the Yu programming language." << '\n';
  cout << '\n';
  cout << "Copyright (C) 2010-2020 MaxXing. License GPLv3.";
  cout << '\n';
}

void ParseArgument(xstl::ArgParser &argp, int argc, const char **argv) {
  const auto ret = argp.Parse(argc, argv);
  // check if need to exit program
  if (argp.GetValue<bool>("help")) {
    argp.PrintHelp();
    std::exit(0);
  }
  if (argp.GetValue<bool>("version")) {
    PrintVersion();
    std::exit(0);
  }
  if (!ret) {
    cerr << "invalid input, run '";
    cerr << argp.program_name() << " -h' for help" << '\n';
    std::exit(1);
  }
}

OutputType GetOutputType(const xstl::ArgParser &argp) {
  const auto out_type = argp.GetValue<string>("out-type");
  int type_index = 0;
  for (const auto &i : {"ast", "yuir", "llvm", "asm", "obj"}) {
    if (out_type == i) return static_cast<OutputType>(type_index);
    ++type_index;
  }
  Logger::LogRawError("invalid output type");
  std::exit(1);
  return OutputType::AST;
}

int GetOptLevel(const xstl::ArgParser &argp) {
  const auto opt_level = argp.GetValue<int>("opt-level");
  if (opt_level < 0 || opt_level > 3) {
    Logger::LogRawError("invalid optimization level");
    std::exit(1);
  }
  return opt_level;
}

void AddDefines(const xstl::ArgParser &argp, LexerManager &lex_man) {
  for (const auto &definition : argp.GetValue<vector<string>>("define")) {
    if (!lex_man.AddDefine(definition)) {
      Logger::LogRawError("invalid macro definition (expected NAME=TEXT)");
      std::exit(1);
    }
  }
}

void InitializeTarget(const xstl::ArgParser &argp, ObjectGen &obj_gen,
                      int opt) {
  obj_gen.set_opt_level(opt);
  obj_gen.set_cpu(argp.GetValue<string>("cpu"));
  obj_gen.set_features(argp.GetValue<string>("features"));
  if (!obj_gen.SetTargetTriple(argp.GetValue<string>("target"))) {
    std::exit(1);
  }
}

bool CompileToIR(const xstl::ArgParser &argp, std::ostream &os,
                 LexerManager &lex_man, IRBuilder &irb, OutputType type) {
  // initialize lexer manager & logger
  const auto file = argp.GetValue<string>("input");
  const auto imp_path = argp.GetValue<vector<string>>("import-path");
  if (!lex_man.LoadSource(file)) return false;
  if (!imp_path.empty()) {
    for (const auto &i : imp_path) {
      if (!lex_man.AddImportPath(1, i)) return false;
    }
  }
  Logger::ResetErrorNum(argp.GetValue<bool>("warn-error"));
  // initialize other stuffs
  Parser parser(lex_man);
  Evaluator eval;
  Analyzer ana(eval);
  // compile source code
  const auto dump_ast = type == OutputType::AST;
  while (auto ast = parser.ParseNext()) {
    // perform semantic analyze
    if (!ast->SemaAnalyze(ana)) break;
    ast->Eval(eval);
    if (Logger::error_num()) break;
    // dump to output
    if (dump_ast) ast->Dump(os);
    // generate IR
    ast->GenerateIR(irb);
  }
  // check if need to exit
  const auto err_num = Logger::error_num();
  return err_num == 0;
}

bool RunPasses(const xstl::ArgParser &argp, std::ostream &os, IRBuilder &irb,
               OutputType type, int opt) {
  // set optimization level
  PassManager pass_man;
  pass_man.set_opt_level(opt);
  // run passes on IR
  if (argp.GetValue<bool>("verbose")) pass_man.ShowInfo(cerr);
  irb.module().RunPasses(pass_man);
  // check if need to dump IR
  const auto dump_yuir = type == OutputType::YuIR;
  const auto err_num = Logger::error_num();
  if (!err_num && dump_yuir) irb.module().Dump(os);
  return err_num == 0;
}

bool GenerateCode(std::ostream &os, IRBuilder &irb, CodeGen &gen,
                  ObjectGen &obj_gen, OutputType type,
                  const std::string &file) {
  // generate code
  irb.module().GenerateCode(gen);
  obj_gen.RunOptimization();
  // check if need to dump code
  if (type != OutputType::LLVM && file.empty()) {
    Logger::LogRawError("output file required when generating asm/obj");
    return false;
  }
  switch (type) {
    case OutputType::LLVM: {
      // dump LLVM IR
      gen.Dump(os);
      break;
    }
    case OutputType::Assembly: {
      // dump assembly
      return obj_gen.GenerateAsm(file);
    }
    case OutputType::Object: {
      // emit object code
      return obj_gen.GenerateObject(file);
    }
    default:;
  }
  return true;
}

}  // namespace

int main(int argc, const char *argv[]) try {
  // set up argument parser
  auto argp = GetArgp();

  // parse argument
  ParseArgument(argp, argc, argv);
  const auto out_type = GetOutputType(argp);
  const auto opt_level = GetOptLevel(argp);

  // initialize output stream
  const auto out_file = argp.GetValue<string>("output");
  const auto write_deps = argp.GetValue<bool>("deps");
  if (write_deps && out_file.empty()) {
    Logger::LogRawError("-MD requires an output file (-o)");
    return 1;
  }
  std::ofstream ofs;
  const auto text_output = out_type == OutputType::AST ||
                           out_type == OutputType::YuIR ||
                           out_type == OutputType::LLVM;
  if (text_output && !out_file.empty()) {
    ofs.open(out_file);
    if (!ofs) {
      Logger::LogRawError("failed to open output file");
      return 1;
    }
  }
  auto &os = ofs.is_open() ? ofs : cout;

  // initialize compilation system
  LexerManager lex_man;
  AddDefines(argp, lex_man);
  IRBuilder irb;
  LLVMGen gen(argp.GetValue<string>("input"));

  // initialize target
  ObjectGen obj_gen(gen.module());
  InitializeTarget(argp, obj_gen, opt_level);
  BaseType::set_ptr_size(obj_gen.GetPointerSize());

  // compile source to target code
  if (!CompileToIR(argp, os, lex_man, irb, out_type)) {
    if (!Logger::error_num()) {
      Logger::LogRawError("invalid input file or import path");
    }
    return 1;
  }
  if (out_type != OutputType::AST) {
    if (!RunPasses(argp, os, irb, out_type, opt_level)) return 1;
    if (out_type != OutputType::YuIR &&
        !GenerateCode(os, irb, gen, obj_gen, out_type, out_file))
      return 1;
  }
  os.flush();
  if (ofs.is_open()) ofs.close();
  if (!os) {
    Logger::LogRawError("failed to write output file");
    return 1;
  }
  if (write_deps &&
      !yulang::front::WriteDepfile(out_file, lex_man.dependencies())) {
    return 1;
  }
  return 0;
} catch (const std::exception &e) {
  PANIC(e.what());
}
