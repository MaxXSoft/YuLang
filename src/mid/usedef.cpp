#include "mid/usedef.h"

using namespace yulang::mid;

void IdManager::ResetId() {
  cur_id_ = 0;
  ids_.clear();
}

std::size_t IdManager::GetId(const Value *val) {
  auto it = ids_.find(val);
  if (it == ids_.end()) {
    auto id = cur_id_++;
    ids_.insert({val, id});
    return id;
  }
  else {
    return it->second;
  }
}

void IdManager::LogName(const Value *val, std::string_view name) {
  if (names_.find(val) == names_.end()) names_.insert({val, name});
}

std::optional<std::string_view> IdManager::GetName(const Value *v) const {
  auto it = names_.find(v);
  if (it != names_.end()) {
    return it->second;
  }
  else {
    return {};
  }
}

void Value::ReplaceBy(const SSAPtr &value) {
  // copy an use list from current value
  auto uses = uses_;
  // reroute all uses to new value
  for (const auto &use : uses) {
    use->set_value(value);
  }
}

void User::RemoveNull() {
  int len = 0;
  for (int i = 0; i < uses_.size(); ++i) {
    if (uses_[i].value()) {
      uses_[len].set_value(uses_[i].value());
      ++len;
    }
  }
  if (len < uses_.size()) Resize(len);
}
