#ifndef YULANG_BACK_IR_H_
#define YULANG_BACK_IR_H_

#include <memory>
#include <any>
#include <vector>
#include <cassert>

namespace yulang::back {

class IRInterface {
 public:
  virtual ~IRInterface() = default;

  // get the exact value of current IR
  virtual const std::any value() const = 0;
};

using IRPtr = std::shared_ptr<IRInterface>;
using IRPtrList = std::vector<IRPtr>;

// cast IR pointer to specific type
template <typename T>
inline T IRCast(const IRPtr &ir) {
  auto ret = ir->value();
  auto value = std::any_cast<T>(&ret);
  assert(value);
  return *value;
}

}  // namespace yulang::back

#endif  // YULANG_BACK_IR_H_
