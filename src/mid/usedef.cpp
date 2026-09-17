#include "mid/usedef.h"

namespace yulang::mid {

void IdManager::ResetId() {
  cur_id_ = 0;
  ids_.clear();
}

std::size_t IdManager::GetId(const Value *val) {
  const auto it = ids_.find(val);
  if (it == ids_.end()) {
    const auto id = cur_id_++;
    ids_.insert({val, id});
    return id;
  }
  return it->second;
}

void IdManager::LogName(const Value *val, std::string_view name) {
  if (names_.find(val) == names_.end()) names_.insert({val, name});
}

std::optional<std::string_view> IdManager::GetName(const Value *v) const {
  const auto it = names_.find(v);
  if (it != names_.end()) {
    return it->second;
  }
  return {};
}

void Value::ReplaceBy(const SSAPtr &value) {
  // copy an use list from current value
  const auto uses = uses_;
  // reroute all uses to new value
  for (const auto &use : uses) {
    use->set_value(value);
  }
}

void User::RemoveNull() {
  std::size_t len = 0;
  for (const auto &use : uses_) {
    if (use.value()) {
      uses_[len].set_value(use.value());
      ++len;
    }
  }
  if (len < uses_.size()) Resize(len);
}

}  // namespace yulang::mid
