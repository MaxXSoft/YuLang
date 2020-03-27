#ifndef YULANG_MID_USEDEF_H_
#define YULANG_MID_USEDEF_H_

// reference: LLVM version 1.3

#include <ostream>
#include <memory>
#include <vector>
#include <forward_list>
#include <cstddef>

#include "define/type.h"

namespace yulang::mid {

// forward declarations
class Value;
class User;
class Use;

using SSAPtr = std::shared_ptr<Value>;
using SSAPtrList = std::vector<SSAPtr>;

// SSA value
class Value {
 public:
  virtual ~Value() = default;

  // dump the content of SSA value to output stream
  virtual void Dump(std::ostream &os) const = 0;

  // add a use reference to current value
  void AddUse(Use *use) { uses_.push_front(use); }
  // remove use reference from current value
  void RemoveUse(Use *use) { uses_.remove(use); }
  // replace current value by another value
  void ReplaceBy(const SSAPtr &value);

  // getters
  const SSAPtr &address() const { return address_; }
  const std::forward_list<Use *> &uses() const { return uses_; }
  const define::TypePtr &type() const { return type_; }

  // setters
  void set_address(const SSAPtr &address) { address_ = address; }
  void set_type(const define::TypePtr &type) { type_ = type; }

 private:
  // address value of current value
  SSAPtr address_;
  // singly-linked list of 'Use'
  std::forward_list<Use *> uses_;
  // type of current value
  define::TypePtr type_;
};

// bidirectional reference between SSA users and values
class Use {
 public:
  explicit Use(const SSAPtr &value, User *user)
      : value_(value), user_(user) {}
  // copy constructor
  Use(const Use &use) : value_(use.value_), user_(use.user_) {
    if (value_) value_->AddUse(this);
  }
  // move constructor
  Use(Use &&use) noexcept
      : value_(std::move(use.value_)), user_(use.user_) {
    if (value_) {
      value_->RemoveUse(&use);
      value_->AddUse(this);
    }
  }
  // destructor
  ~Use() {
    if (value_) value_->RemoveUse(this);
  }

  // setters
  void set_value(const SSAPtr &value) {
    if (value_) value_->RemoveUse(this);
    value_ = value;
    if (value_) value_->AddUse(this);
  }

  // getters
  const SSAPtr &value() const { return value_; }
  User *user() const { return user_; }

 private:
  SSAPtr value_;
  User *user_;
};

// SSA user which can use other values
class User : public Value {
 public:
  // preallocate some space for values
  void Reserve(std::size_t size) { values_.reserve(size); }
  // add new value to current user
  void AddValue(const SSAPtr &value) {
    values_.push_back(Use(value, this));
  }

  // access value in current user
  Use &operator[](std::size_t pos) { return values_[pos]; }
  // access value in current user (const)
  const Use &operator[](std::size_t pos) const { return values_[pos]; }
  // begin iterator
  auto begin() { return values_.begin(); }
  auto begin() const { return values_.begin(); }
  // end iterator
  auto end() { return values_.end(); }
  auto end() const { return values_.end(); }

  // getters
  // count of elements in current user
  std::size_t size() const { return values_.size(); }
  // return true if no value in current user
  bool empty() const { return values_.empty(); }

 private:
  std::vector<Use> values_;
};

}  // namespace yulang::mid

#endif  // YULANG_MID_USEDEF_H_
