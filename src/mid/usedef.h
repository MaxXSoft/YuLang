#ifndef YULANG_MID_USEDEF_H_
#define YULANG_MID_USEDEF_H_

// reference: LLVM version 1.3

#include <memory>
#include <vector>
#include <ostream>
#include <forward_list>
#include <any>
#include <cstddef>

#include "define/type.h"

namespace yulang::mid {

// forward declarations
class Value;
class User;
class Use;

using SSAPtr = std::shared_ptr<Value>;
using SSAPtrList = std::vector<SSAPtr>;
using UserPtr = std::shared_ptr<User>;
using UserPtrList = std::vector<UserPtr>;

// SSA value
class Value {
 public:
  virtual ~Value() = default;

  // dump the content of SSA value to output stream
  virtual void Dump(std::ostream &os) const = 0;
  // get address value of current value
  virtual SSAPtr GetAddr() const { return nullptr; }

  // add a use reference to current value
  void AddUse(Use *use) { uses_.push_front(use); }
  // remove use reference from current value
  void RemoveUse(Use *use) { uses_.remove(use); }
  // replace current value by another value
  void ReplaceBy(const SSAPtr &value);

  // getters
  const define::TypePtr &type() const { return type_; }
  const std::any &metadata() const { return metadata_; }
  const std::forward_list<Use *> &uses() const { return uses_; }

  // setters
  void set_type(const define::TypePtr &type) { type_ = type; }
  void set_metadata(const std::any &metadata) { metadata_ = metadata; }

 private:
  // singly-linked list of 'Use'
  std::forward_list<Use *> uses_;
  // type of current value
  define::TypePtr type_;
  // metadata
  std::any metadata_;
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
  void Reserve(std::size_t size) { uses_.reserve(size); }
  // add new value to current user
  void AddValue(const SSAPtr &value) {
    uses_.push_back(Use(value, this));
  }

  // access value in current user
  Use &operator[](std::size_t pos) { return uses_[pos]; }
  // access value in current user (const)
  const Use &operator[](std::size_t pos) const { return uses_[pos]; }
  // begin iterator
  auto begin() { return uses_.begin(); }
  auto begin() const { return uses_.begin(); }
  // end iterator
  auto end() { return uses_.end(); }
  auto end() const { return uses_.end(); }

  // getters
  // count of elements in current user
  std::size_t size() const { return uses_.size(); }
  // return true if no value in current user
  bool empty() const { return uses_.empty(); }
  // list of all uses
  std::vector<Use> &uses() { return uses_; }
  // constant list of all uses
  const std::vector<Use> &uses() const { return uses_; }

 private:
  std::vector<Use> uses_;
};

}  // namespace yulang::mid

#endif  // YULANG_MID_USEDEF_H_
