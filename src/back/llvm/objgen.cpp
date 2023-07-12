#include "back/llvm/objgen.h"

#include <system_error>
#include <memory>
#include <cassert>

#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/IR/Type.h"
#include "llvm/Config/llvm-config.h"

#if LLVM_VERSION_MAJOR >= 14
#include "llvm/MC/TargetRegistry.h"
#else
#include "llvm/Support/TargetRegistry.h"
#endif

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
#if LLVM_VERSION_MAJOR >= 10
  auto flags = llvm::sys::fs::OF_None;
  if (type == CodeGenFileType::Asm) flags |= llvm::sys::fs::OF_Text;
#else
  auto flags = llvm::sys::fs::F_None;
  if (type == CodeGenFileType::Asm) flags |= llvm::sys::fs::F_Text;
#endif
  auto out = std::make_unique<llvm::ToolOutputFile>(file, ec, flags);
  if (ec) {
    Logger::LogRawError("failed to open output file");
    Logger::LogRawError(ec.message());
    return false;
  }
  // get file type
#if LLVM_VERSION_MAJOR >= 10
  auto file_type = type == CodeGenFileType::Asm
                       ? llvm::CodeGenFileType::CGFT_AssemblyFile
                       : llvm::CodeGenFileType::CGFT_ObjectFile;
#else
  auto file_type =
      type == CodeGenFileType::Asm
          ? llvm::TargetMachine::CodeGenFileType::CGFT_AssemblyFile
          : llvm::TargetMachine::CodeGenFileType::CGFT_ObjectFile;
#endif
  // compile to object file
  llvm::legacy::PassManager pass;
  if (machine_->addPassesToEmitFile(pass, out->os(), nullptr, file_type)) {
    Logger::LogRawError("target machine cannot emit file of this type");
    return false;
  }
  pass.run(*module_);
  out->keep();
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
  machine_ = target->createTargetMachine(tt, cpu_, features_, opt, rm);
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
  auto ptr = llvm::Type::getInt8PtrTy(module_->getContext());
  return module_->getDataLayout().getTypeAllocSize(ptr);
}
