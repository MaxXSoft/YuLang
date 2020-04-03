#ifndef YULANG_BACK_LLVM_OBJGEN_H_
#define YULANG_BACK_LLVM_OBJGEN_H_

#include <ostream>
#include <string>
#include <cstddef>

#include "llvm/Target/TargetMachine.h"

#include "back/llvm/define.h"

namespace yulang::back::ll {

class ObjectGen {
 public:
  ObjectGen(const ModulePtr &module) : module_(module), machine_(nullptr) {
    InitTarget();
  }

  // run optimization on specific module
  void RunOptimization();
  // set target triple
  bool SetTargetTriple(const std::string &triple);
  // generate assembly language file
  bool GenerateAsm(const std::string &file);
  // generate object file
  bool GenerateObject(const std::string &file);
  // get pointer size of current target
  std::size_t GetPointerSize() const;

  // setters
  void set_opt_level(std::size_t opt_level) { opt_level_ = opt_level; }

  // getters
  const std::string &target_triple() const {
    return module_->getTargetTriple();
  }

 private:
  void InitTarget();
  bool GenerateTargetCode(const std::string &file,
                          llvm::TargetMachine::CodeGenFileType type);

  // LLVM module
  const ModulePtr &module_;
  // optimization level
  std::size_t opt_level_;
  // target info
  llvm::TargetMachine *machine_;
};

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_OBJGEN_H_
