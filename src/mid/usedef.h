#ifndef YULANG_MID_USEDEF_H_
#define YULANG_MID_USEDEF_H_

// reference: LLVM version 1.3

#include <memory>
#include <vector>
#include <ostream>
#include <list>
#include <any>
#include <unordered_map>
#include <string_view>
#include <optional>
#include <cstddef>

#include "front/logger.h"
#include "define/type.h"

// forward declarations for visitor method
namespace yulang {
namespace mid {
class PassBase;
}  // namespace mid

namespace back {
class CodeGenInterface;
// alias for 'CodeGenInterface'
using CodeGen = CodeGenInterface;
}  // namespace back
}  // namespace yulang

namespace yulang::mid {

// forward declarations of SSA IRs
class Value;
class User;
class Use;

using SSAPtr = std::shared_ptr<Value>;
using SSAPtrList = std::list<SSAPtr>;
using UserPtr = std::shared_ptr<User>;
using UserPtrList = std::list<UserPtr>;

// utility class for dumping SSA IR
class IdManager {
 public:
  IdManager() : cur_id_(0) {}

  // reset status about identifier
  void ResetId();
  // get id of specific value
  std::size_t GetId(const Value *val);
  // get id of specific value
  std::size_t GetId(const SSAPtr &val) { return GetId(val.get()); }
  // get name of specific value, set if not found
  void LogName(const Value *val, std::string_view name);
  // get name of specific value
  std::optional<std::string_view> GetName(const Value *val) const;
  // get name of specific value
  std::optional<std::string_view> GetName(const SSAPtr &val) const {
    return GetName(val.get());
  }

 private:
  std::size_t cur_id_;
  std::unordered_map<const Value *, std::size_t> ids_;
  std::unordered_map<const Value *, std::string_view> names_;
};

// SSA value
class Value {
 public:
  virtual ~Value() = default;

  // dump the content of SSA value to output stream
  virtual void Dump(std::ostream &os, IdManager &idm) const = 0;
  // run pass on current SSA value
  virtual void RunPass(PassBase &pass) = 0;
  // run code generation
  virtual void GenerateCode(back::CodeGen &pass) = 0;
  // get address value of current value
  virtual SSAPtr GetAddr() const { return nullptr; }

  // add a use reference to current value
  void AddUse(Use *use) { uses_.push_back(use); }
  // remove use reference from current value
  void RemoveUse(Use *use) { uses_.remove(use); }
  // replace current value by another value
  void ReplaceBy(const SSAPtr &value);

  // getters
  const front::LogPtr &logger() const { return logger_; }
  const define::TypePtr &type() const { return type_; }
  const std::any &metadata() const { return metadata_; }
  const std::list<Use *> &uses() const { return uses_; }

  // setters
  void set_logger(const front::LogPtr &logger) { logger_ = logger; }
  void set_type(const define::TypePtr &type) { type_ = type; }
  void set_metadata(const std::any &metadata) { metadata_ = metadata; }

 private:
  // pointer to logger
  front::LogPtr logger_;
  // type of current value
  define::TypePtr type_;
  // metadata
  std::any metadata_;
  // linked list of 'Use'
  std::list<Use *> uses_;
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

 private:
  std::vector<Use> uses_;
};

}  // namespace yulang::mid

#endif  // YULANG_MID_USEDEF_H_
