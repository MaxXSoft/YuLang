#include "mid/passman.h"

#include <iomanip>
#include <cassert>

using namespace yulang::mid;

// definition of static member in 'PassManager'
std::vector<PassInfo *> PassManager::passes_;

void PassManager::RunPasses() const {
  bool changed = true;
  // run until nothing changes
  while (changed) {
    changed = false;
    // traverse all passes
    for (const auto &info : passes_) {
      if (info->min_opt_level() < opt_level_) continue;
      const auto &pass = info->pass();
      // handle by pass type
      if (pass->IsModulePass()) {
        // run on global values
        if (pass->RunOnModule(*vars_)) changed = true;
        if (pass->RunOnModule(*funcs_)) changed = true;
      }
      else if (pass->IsFunctionPass()) {
        // traverse all functions
        for (const auto &func : *funcs_) {
          if (pass->RunOnFunction(func)) changed = true;
        }
      }
      else {
        assert(pass->IsBlockPass());
        // traverse all basic blocks
        for (const auto &func : *funcs_) {
          for (const auto &i : *func) {
            auto block = std::static_pointer_cast<BlockSSA>(i.value());
            if (pass->RunOnBlock(block)) changed = true;
          }
        }
      }
    }
  }
}

void PassManager::ShowInfo(std::ostream &os) const {
  // display optimization level
  os << "current optimization level: " << opt_level_ << std::endl;
  os << std::endl;
  // show registed info
  os << "registed passes:" << std::endl;
  if (passes_.empty()) {
    os << "  <none>" << std::endl;
    return;
  }
  for (const auto &i : passes_) {
    os << "  ";
    os << std::setw(32) << std::left << i->name();
    os << std::setw(18) << std::left
       << "min_opt_level = " << i->min_opt_level();
    os << "is_analysis = " << std::boolalpha << i->is_analysis();
    os << std::endl;
  }
  os << std::endl;
  // show enabled passes
  int count = 0;
  os << "enabled passes:" << std::endl;
  for (const auto &i : passes_) {
    if (opt_level_ >= i->min_opt_level()) {
      if (count % 5 == 0) os << "  ";
      os << std::setw(16) << std::left << i->name();
      if (count % 5 == 4) os << std::endl;
      ++count;
    }
  }
  if (!count) {
    os << "  <none>" << std::endl;
  }
  else if ((count - 1) % 5 != 4) {
    os << std::endl;
  }
}
