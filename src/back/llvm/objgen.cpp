#include "back/llvm/objgen.h"

#include <system_error>

#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Type.h"

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

bool ObjectGen::GenerateTargetCode(
    const std::string &file, llvm::TargetMachine::CodeGenFileType type) {
  // open object file
  std::error_code ec;
  llvm::raw_fd_ostream raw(file, ec, llvm::sys::fs::F_None);
  if (ec) {
    Logger::LogRawError("failed to open output file");
    Logger::LogRawError(ec.message());
    return false;
  }
  // compile to object file
  llvm::legacy::PassManager pass;
  if (machine_->addPassesToEmitFile(pass, raw, nullptr, type)) {
    Logger::LogRawError("target machine cannot emit file of this type");
    return false;
  }
  pass.run(*module_);
  raw.flush();
  return true;
}

void ObjectGen::RunOptimization() {
  // initialize pass manager
  llvm::legacy::PassManager pm;
  llvm::PassManagerBuilder builder;
  builder.OptLevel = opt_level_;
  builder.SizeLevel = 0;
  builder.Inliner = llvm::createFunctionInliningPass();
  builder.DisableUnrollLoops = false;
  builder.DisableTailCalls = false;
  builder.LoopVectorize = true;
  builder.SLPVectorize = true;
  builder.populateModulePassManager(pm);
  // run pass on module
  pm.run(*module_);
}

bool ObjectGen::SetTargetTriple(const std::string &triple) {
  // set triple
  auto tt = triple;
  if (tt.empty()) {
    tt = llvm::sys::getDefaultTargetTriple();
  }
  // get target info
  std::string error_msg;
  auto target = llvm::TargetRegistry::lookupTarget(tt, error_msg);
  if (!target) {
    Logger::LogRawError(error_msg);
    return false;
  }
  module_->setTargetTriple(tt);
  // initialize target machine
  llvm::TargetOptions opt;
  auto rm = llvm::Optional<llvm::Reloc::Model>();
  machine_ = target->createTargetMachine(tt, "generic", "", opt, rm);
  module_->setDataLayout(machine_->createDataLayout());
  return true;
}

bool ObjectGen::GenerateAsm(const std::string &file) {
  auto file_type = llvm::TargetMachine::CGFT_AssemblyFile;
  return GenerateTargetCode(file, file_type);
}

bool ObjectGen::GenerateObject(const std::string &file) {
  auto file_type = llvm::TargetMachine::CGFT_ObjectFile;
  return GenerateTargetCode(file, file_type);
}

std::size_t ObjectGen::GetPointerSize() const {
  auto ptr = llvm::Type::getInt8PtrTy(module_->getContext());
  return module_->getDataLayout().getTypeAllocSize(ptr);
}
