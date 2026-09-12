#include "back/llvm/objgen.h"

#include <system_error>
#include <memory>
#include <cassert>
#include <optional>

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/MC/TargetRegistry.h"

#include "front/logger.h"

using namespace yulang::front;
using namespace yulang::back::ll;

void ObjectGen::InitTarget() {
  // initialize target registry
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();
}

bool ObjectGen::GenerateTargetCode(const std::string &file,
                                   CodeGenFileType type) {
  // open object file
  std::error_code ec;
  auto flags = llvm::sys::fs::OF_None;
  if (type == CodeGenFileType::Asm) flags |= llvm::sys::fs::OF_Text;
  auto out = std::make_unique<llvm::ToolOutputFile>(file, ec, flags);
  if (ec) {
    Logger::LogRawError("failed to open output file");
    Logger::LogRawError(ec.message());
    return false;
  }
  // get file type
  auto file_type = type == CodeGenFileType::Asm
                       ? llvm::CodeGenFileType::AssemblyFile
                       : llvm::CodeGenFileType::ObjectFile;
  // Use LLVM's target-specific code generation pipeline, as llc does.
  llvm::legacy::PassManager pass;
  if (machine_->addPassesToEmitFile(pass, out->os(), nullptr, file_type,
                                   /*DisableVerify=*/false)) {
    Logger::LogRawError("target machine cannot emit file of this type");
    return false;
  }
  pass.run(*module_);
  out->keep();
  return true;
}

void ObjectGen::RunOptimization() {
  llvm::LoopAnalysisManager lam;
  llvm::FunctionAnalysisManager fam;
  llvm::CGSCCAnalysisManager cgam;
  llvm::ModuleAnalysisManager mam;
  llvm::PassBuilder builder(machine_);
  builder.registerModuleAnalyses(mam);
  builder.registerCGSCCAnalyses(cgam);
  builder.registerFunctionAnalyses(fam);
  builder.registerLoopAnalyses(lam);
  builder.crossRegisterProxies(lam, fam, cgam, mam);

  const llvm::OptimizationLevel levels[] = {
      llvm::OptimizationLevel::O0, llvm::OptimizationLevel::O1,
      llvm::OptimizationLevel::O2, llvm::OptimizationLevel::O3};
  assert(opt_level_ < 4);
  auto level = levels[opt_level_];
  auto pm = opt_level_ == 0 ? builder.buildO0DefaultPipeline(level)
                            : builder.buildPerModuleDefaultPipeline(level);
  pm.run(*module_, mam);
}

bool ObjectGen::SetTargetTriple(const std::string &triple) {
  // set triple
  auto tt = triple;
  if (tt.empty()) {
    tt = llvm::sys::getDefaultTargetTriple();
  }
  llvm::Triple target_triple(tt);
  // get target info
  std::string error_msg;
  auto target = llvm::TargetRegistry::lookupTarget(target_triple, error_msg);
  if (!target) {
    Logger::LogRawError(error_msg);
    return false;
  }
  module_->setTargetTriple(target_triple);
  // initialize target machine
  llvm::TargetOptions opt;
  assert(opt_level_ < 4);
  auto codegen_level = llvm::CodeGenOpt::getLevel(static_cast<int>(opt_level_));
  machine_ = target->createTargetMachine(target_triple, cpu_, features_, opt,
                                        std::nullopt, std::nullopt,
                                        *codegen_level);
  module_->setDataLayout(machine_->createDataLayout());
  return true;
}

bool ObjectGen::GenerateAsm(const std::string &file) {
  return GenerateTargetCode(file, CodeGenFileType::Asm);
}

bool ObjectGen::GenerateObject(const std::string &file) {
  return GenerateTargetCode(file, CodeGenFileType::Object);
}

std::size_t ObjectGen::GetPointerSize() const {
  return module_->getDataLayout().getPointerSize();
}
