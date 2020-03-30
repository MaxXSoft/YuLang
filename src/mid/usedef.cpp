#include "mid/usedef.h"

using namespace yulang::mid;

void IdManager::Reset() {
  cur_id_ = 0;
  ids_.clear();
}

std::size_t IdManager::Log(const Value *val) {
  auto id = cur_id_++;
  assert(ids_.find(val) == ids_.end());
  ids_.insert({val, id});
  return id;
}

std::size_t IdManager::GetId(const Value *val) const {
  auto it = ids_.find(val);
  assert(it != ids_.end());
  return it->second;
}

void Value::ReplaceBy(const SSAPtr &value) {
  // copy an use list from current value
  auto uses = uses_;
  // reroute all uses to new value
  for (const auto &use : uses) {
    use->set_value(value);
  }
}
