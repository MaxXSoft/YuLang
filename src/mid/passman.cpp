#include "mid/passman.h"

#include <iomanip>

using namespace yulang::mid;

// definition of static member in 'PassManager'
std::vector<PassInfo *> PassManager::passes_;

void PassManager::RunPasses() const {
  //
}

void PassManager::ShowInfo(std::ostream &os) const {
  //
}
