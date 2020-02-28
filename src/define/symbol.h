#ifndef YULANG_DEFINE_SYMBOL_H_
#define YULANG_DEFINE_SYMBOL_H_

#include <string>
#include <variant>
#include <optional>

#include "define/type.h"
#include "xstl/nested.h"

namespace yulang::define {

// scope environment (symbol table)
using EnvPtr = xstl::NestedMapPtr<std::string, TypePtr>;

// evaluated values
using EvalNum = std::variant<std::uint32_t, float, double>;
using EvalEnvPtr = xstl::NestedMapPtr<std::string, std::optional<EvalNum>>;

// make a new environment
inline EnvPtr MakeEnv() {
  return xstl::MakeNestedMap<std::string, TypePtr>();
}

// make a new environment (with outer environment)
inline EnvPtr MakeEnv(const EnvPtr &outer) {
  return xstl::MakeNestedMap<std::string, TypePtr>(outer);
}

// make a new evaluation environment
inline EvalEnvPtr MakeEvalEnv() {
  return xstl::MakeNestedMap<std::string, std::optional<EvalNum>>();
}

// make a new environment (with outer environment)
inline EvalEnvPtr MakeEvalEnv(const EvalEnvPtr &outer) {
  return xstl::MakeNestedMap<std::string, std::optional<EvalNum>>(outer);
}

}  // namespace yulang::define

#endif  // YULANG_DEFINE_SYMBOL_H_
