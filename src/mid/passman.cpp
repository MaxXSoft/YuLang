#include "mid/passman.h"

#include <cassert>
#include <iomanip>

namespace yulang::mid {

std::list<PassInfo *> &PassManager::GetPasses() {
  static std::list<PassInfo *> passes;
  return passes;
}

void PassManager::RegisterPass(PassInfo *info) { GetPasses().push_back(info); }

// The fixed-point traversal follows the module/function/block pass hierarchy.
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void PassManager::RunPasses() const {
  bool changed = true;
  // run until nothing changes
  while (changed) {
    changed = false;
    // traverse all passes
    for (const auto &info : GetPasses()) {
      if (info->min_opt_level() > opt_level_) continue;
      const auto &pass = info->pass();
      // handle by pass type
      if (pass->IsModulePass()) {
        // run on global values
        if (pass->RunOnModule(*vars_)) changed = true;
        if (pass->RunOnModule(*funcs_)) changed = true;
      } else if (pass->IsFunctionPass()) {
        // traverse all functions
        for (const auto &func : *funcs_) {
          if (pass->RunOnFunction(func)) changed = true;
        }
      } else {
        assert(pass->IsBlockPass());
        // traverse all basic blocks
        for (const auto &func : *funcs_) {
          for (const auto &i : *func) {
            const auto block = std::static_pointer_cast<BlockSSA>(i.value());
            if (pass->RunOnBlock(block)) changed = true;
          }
        }
      }
    }
  }
}

void PassManager::ShowInfo(std::ostream &os) const {
  // display optimization level
  os << "current optimization level: " << opt_level_ << '\n';
  os << '\n';
  // show registed info
  os << "registed passes:" << '\n';
  if (GetPasses().empty()) {
    os << "  <none>" << '\n';
    return;
  }
  for (const auto &i : GetPasses()) {
    os << "  ";
    os << std::setw(20) << std::left << i->name();
    os << "min_opt_level = " << i->min_opt_level() << ", ";
    os << "is_analysis = " << std::boolalpha << i->is_analysis();
    os << '\n';
  }
  os << '\n';
  // show enabled passes
  int count = 0;
  os << "enabled passes:" << '\n';
  for (const auto &i : GetPasses()) {
    if (opt_level_ >= i->min_opt_level()) {
      if (count % 5 == 0) os << "  ";
      os << std::setw(16) << std::left << i->name();
      if (count % 5 == 4) os << '\n';
      ++count;
    }
  }
  if (!count) {
    os << "  <none>" << '\n';
  } else if ((count - 1) % 5 != 4) {
    os << '\n';
  }
}

}  // namespace yulang::mid
