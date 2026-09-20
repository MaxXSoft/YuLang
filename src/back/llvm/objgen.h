#ifndef YULANG_BACK_LLVM_OBJGEN_H_
#define YULANG_BACK_LLVM_OBJGEN_H_

#include <cstddef>
#include <cstdint>
#include <ostream>
#include <string>

#include "back/llvm/define.h"
#include "llvm/Target/TargetMachine.h"

namespace yulang::back::ll {

class ObjectGen {
 public:
  explicit ObjectGen(const ModulePtr &module) : module_(module) {
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
  [[nodiscard]] std::size_t GetPointerSize() const;
  // Configure language type alignment from the selected target's ABI.
  void ConfigureTypeLayout() const;

  // setters
  void set_opt_level(std::size_t opt_level) { opt_level_ = opt_level; }
  void set_cpu(const std::string &cpu) { cpu_ = cpu; }
  void set_features(const std::string &features) { features_ = features; }

 private:
  // file type of code generation
  enum class CodeGenFileType : std::uint8_t {
    Asm,
    Object,
  };

  static void InitTarget();
  bool GenerateTargetCode(const std::string &file, CodeGenFileType type);

  // LLVM module
  // This service is bound to its owner for its entire lifetime.
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  const ModulePtr &module_;
  // optimization level
  std::size_t opt_level_{0};
  // target info
  llvm::TargetMachine *machine_{nullptr};
  // CPU & features
  std::string cpu_, features_;
};

}  // namespace yulang::back::ll

#endif  // YULANG_BACK_LLVM_OBJGEN_H_
