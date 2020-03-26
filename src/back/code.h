#ifndef YULANG_BACK_CODE_H_
#define YULANG_BACK_CODE_H_

#include <memory>
#include <any>
#include <vector>
#include <cassert>

namespace yulang::back {

class CodeInterface {
 public:
  virtual ~CodeInterface() = default;

  // get the exact value of current code
  virtual std::any value() const = 0;
};

using CodePtr = std::shared_ptr<CodeInterface>;
using CodePtrList = std::vector<CodePtr>;

// cast code pointer to specific type
template <typename T>
inline T CodeCast(const CodePtr &ir) {
  auto ret = ir->value();
  auto value = std::any_cast<T>(&ret);
  assert(value);
  return *value;
}

}  // namespace yulang::back

#endif  // YULANG_BACK_CODE_H_
