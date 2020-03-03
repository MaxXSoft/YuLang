#ifndef YULANG_DEFINE_TYPE_H_
#define YULANG_DEFINE_TYPE_H_

#include <memory>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <optional>
#include <cstdint>
#include <cstddef>
#include <cassert>

#include "define/token.h"

namespace yulang::define {

// definition of base class of all types
class BaseType;
using TypePtr = std::shared_ptr<BaseType>;
using TypePtrList = std::vector<TypePtr>;
using TypePair = std::pair<std::string, TypePtr>;
using TypePairList = std::vector<TypePair>;

class BaseType {
 public:
  virtual ~BaseType() = default;

  // return true if is right value
  virtual bool IsRightValue() const = 0;
  // return true if is void type
  virtual bool IsVoid() const = 0;
  // return true if is basic type
  // i.e. is primitive type, function, pointer, or their reference
  virtual bool IsBasic() const = 0;
  // return true if is integer
  virtual bool IsInteger() const = 0;
  // return true if is unsigned
  virtual bool IsUnsigned() const = 0;
  // return true if is floating point
  virtual bool IsFloat() const = 0;
  // return true if is boolean
  virtual bool IsBool() const = 0;
  // return true if is constant type
  virtual bool IsConst() const = 0;
  // return true if is function type
  virtual bool IsFunction() const = 0;
  // return true if is array type
  virtual bool IsArray() const = 0;
  // return true if is pointer type
  virtual bool IsPointer() const = 0;
  // return true if is reference type
  virtual bool IsReference() const = 0;
  // return true if left value which is current type
  // can accept the right value which is specific type
  virtual bool CanAccept(const TypePtr &type) const = 0;
  // return true if current type can be casted to specific type
  virtual bool CanCastTo(const TypePtr &type) const = 0;
  // return true if two types are identical (ignore left/right value)
  virtual bool IsIdentical(const TypePtr &rhs) const = 0;
  // return the size of current type
  virtual std::size_t GetSize() const = 0;
  // return the type of arguments of a function call
  virtual std::optional<TypePtrList> GetArgsType() const = 0;
  // return the return type of a function call
  virtual TypePtr GetReturnType(const TypePtrList &args) const = 0;
  // return the length of current type
  // e.g. array length, struct field number
  virtual std::size_t GetLength() const = 0;
  // return the element at specific index
  virtual const TypePtr &GetElem(std::size_t index) const = 0;
  // return the dereferenced type of current type
  virtual TypePtr GetDerefedType() const = 0;
  // return the identifier of current type
  virtual std::string GetTypeId() const = 0;
  // return a new type with specific value type (left/right)
  virtual TypePtr GetValueType(bool is_right) = 0;
};

class PrimType : public BaseType {
 public:
  enum class Type {
    Void, Null,
    Int8, Int16, Int32, Int64,
    UInt8, UInt16, UInt32, UInt64,
    Bool, Float32, Float64,
  };

  PrimType(Type type, bool is_right) : type_(type), is_right_(is_right) {}

 private:
  Type type_;
  bool is_right_;
};

class SturctType : public BaseType {
 public:
  SturctType(TypePairList elems) : elems_(std::move(elems)) {}

 private:
  TypePairList elems_;
};

class EnumType : public BaseType {
 public:
  using ElemSet = std::unordered_set<std::string>;

  EnumType(TypePtr type, ElemSet elems) : elems_(std::move(elems)) {}

 private:
  ElemSet elems_;
};

class ConstType : public BaseType {
 public:
  ConstType(TypePtr type) : type_(type) {}

 private:
  TypePtr type_;
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
  ArrayType(TypePtr base, std::size_t len)
      : base_(std::move(base)), len_(len) {}

 private:
  TypePtr base_;
  std::size_t len_;
};

class PointerType : public BaseType {
 public:
  PointerType(TypePtr base) : base_(std::move(base)) {}

 private:
  TypePtr base_;
};

class RefType : public BaseType {
 public:
  RefType(TypePtr base) : base_(std::move(base)) {}

 private:
  TypePtr base_;
};

// create a new primitive type by keyword
inline TypePtr MakePrimType(Keyword key, bool is_right) {
  using Type = PrimType::Type;
  Type type;
  switch (key) {
    case Keyword::Null: type = Type::Null; break;
    case Keyword::Int8: type = Type::Int8; break;
    case Keyword::Int16: type = Type::Int16; break;
    case Keyword::Int32: type = Type::Int32; break;
    case Keyword::Int64: type = Type::Int64; break;
    case Keyword::UInt8: type = Type::UInt8; break;
    case Keyword::UInt16: type = Type::UInt16; break;
    case Keyword::UInt32: type = Type::UInt32; break;
    case Keyword::UInt64: type = Type::UInt64; break;
    case Keyword::Bool: type = Type::Bool; break;
    case Keyword::Float32: type = Type::Float32; break;
    case Keyword::Float64: type = Type::Float64; break;
    default: assert(false); return nullptr;
  }
  return std::make_shared<PrimType>(type, is_right);
}

// create a new void type
inline TypePtr MakeVoid() {
  std::make_shared<PrimType>(PrimType::Type::Void);
}

}  // namespace yulang::define

#endif  // YULANG_DEFINE_TYPE_H_
