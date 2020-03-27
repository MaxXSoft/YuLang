#include "mid/usedef.h"

using namespace yulang::mid;

void Value::ReplaceBy(const SSAPtr &value) {
  // copy an use list from current value
  auto uses = uses_;
  // reroute all uses to new value
  for (const auto &use : uses) {
    use->set_value(value);
  }
}
