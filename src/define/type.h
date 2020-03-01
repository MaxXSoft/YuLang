#ifndef YULANG_DEFINE_TYPE_H_
#define YULANG_DEFINE_TYPE_H_

#include <memory>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <cstdint>
#include <cstddef>

#include "define/token.h"

namespace yulang::define {

// definition of base class of all types
class BaseType;
using TypePtr = std::shared_ptr<BaseType>;
using TypePtrList = std::vector<TypePtr>;
using TypePtrMap = std::unordered_map<std::string, TypePtr>;

class BaseType {
 public:
  virtual ~BaseType() = default;

  // return true if is integer
  virtual bool IsInteger() const = 0;
  // return true if is unsigned
  virtual bool IsUnsigned() const = 0;
  // return true if is floating point
  virtual bool IsFloat() const = 0;
  // return true if left value which is current type
  // can accept the right value which is specific type
  virtual bool CanAccept(const TypePtr &type) const = 0;
  // return true if current type can be casted to specific type
  virtual bool CanCastTo(const TypePtr &type) const = 0;
  // return the size of current type
  virtual std::size_t GetSize() const = 0;
};

class PrimType : public BaseType {
 public:
  PrimType(Keyword type) : type_(type) {}

 private:
  Keyword type_;
};

class SturctType : public BaseType {
 public:
  SturctType(TypePtrMap elems) : elems_(std::move(elems)) {}

 private:
  TypePtrMap elems_;
};

class EnumType : public BaseType {
 public:
  using ElemSet = std::unordered_set<std::string>;

  EnumType(TypePtr type, ElemSet elems) : elems_(std::move(elems)) {}

 private:
  ElemSet elems_;
};

class FuncType : public BaseType {
 public:
  FuncType(TypePtrList args, TypePtr ret)
      : args_(std::move(args)), ret_(std::move(ret)) {}

 private:
  TypePtrList args_;
  TypePtr ret_;
};

class VolaType : public BaseType {
 public:
  VolaType(TypePtr type) : type_(std::move(type)) {}

 private:
  TypePtr type_;
};

class ArrayType : public BaseType {
 public:
  ArrayType(TypePtr base, std::uint32_t len)
      : base_(std::move(base)), len_(len) {}

 private:
  TypePtr base_;
  std::uint32_t len_;
};

class PointerType : public BaseType {
 public:
  PointerType(bool is_var, TypePtr base)
      : is_var_(is_var), base_(std::move(base)) {}

 private:
  bool is_var_;
  TypePtr base_;
};

class RefType : public BaseType {
 public:
  RefType(bool is_var, TypePtr base)
      : is_var_(is_var), base_(std::move(base)) {}

 private:
  bool is_var_;
  TypePtr base_;
};

}  // namespace yulang::define

#endif  // YULANG_DEFINE_TYPE_H_
