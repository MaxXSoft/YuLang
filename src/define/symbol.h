#ifndef YULANG_DEFINE_SYMBOL_H_
#define YULANG_DEFINE_SYMBOL_H_

#include <string>

#include "define/type.h"
#include "xstl/nested.h"

namespace yulang::define {

// scope environment (symbol table)
using EnvPtr = xstl::NestedMapPtr<std::string, TypePtr>;

// make a new environment
inline EnvPtr MakeEnvironment() {
  return xstl::MakeNestedMap<std::string, TypePtr>();
}

// make a new environment (with outer environment)
inline EnvPtr MakeEnvironment(const EnvPtr &outer) {
  return xstl::MakeNestedMap<std::string, TypePtr>(outer);
}

}  // namespace yulang::define

#endif  // YULANG_DEFINE_SYMBOL_H_
