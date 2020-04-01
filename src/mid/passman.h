#ifndef YULANG_MID_PASSMAN_H_
#define YULANG_MID_PASSMAN_H_

#include <string_view>
#include <ostream>
#include <vector>
#include <memory>
#include <cstddef>

#include "mid/pass.h"
#include "mid/module.h"

namespace yulang::mid {

// pass information
class PassInfo {
 public:
  PassInfo(std::string_view name, PassPtr pass, std::size_t min_opt_level,
           bool is_analysis)
      : name_(name), pass_(std::move(pass)),
        min_opt_level_(min_opt_level), is_analysis_(is_analysis) {}
  virtual ~PassInfo() = default;

  // getters
  std::string_view name() const { return name_; }
  const PassPtr &pass() const { return pass_; }
  std::size_t min_opt_level() const { return min_opt_level_; }
  bool is_analysis() const { return is_analysis_; }

 private:
  std::string_view name_;
  PassPtr pass_;
  std::size_t min_opt_level_;
  bool is_analysis_;
};

// pass manager for all SSA IR passes
class PassManager {
 public:
  PassManager() : opt_level_(0) {}

  // register a new pass
  static void RegisterPass(PassInfo *info) { passes_.push_back(info); }

  // run all passes on specific module
  void RunPasses(const Module &module);
  // show info of passes
  void ShowInfo(std::ostream &os) const;

  // setters
  void set_opt_level(std::size_t opt_level) { opt_level_ = opt_level; }

  // getters
  std::size_t opt_level() const { return opt_level_; }

 private:
  static std::vector<PassInfo *> passes_;
  std::size_t opt_level_;
};

// helper class for registering a pass
template <typename T>
class RegisterPass : public PassInfo {
 public:
  RegisterPass(std::string_view name, std::size_t min_opt_level,
               bool is_analysis)
      : PassInfo(name, std::make_unique<T>(), min_opt_level, is_analysis) {
    PassManager::RegisterPass(this);
  }
};

// register a pass
#define REGISTER_PASS(cls, name, min_opt_level, is_analysis) \
  static RegisterPass<cls> __pass_##name(#name, min_opt_level, is_analysis)

}  // namespace yulang::mid

#endif  // YULANG_MID_PASSMAN_H_
