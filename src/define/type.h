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
  // return true if is null type
  virtual bool IsNull() const = 0;
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
  // return true if is structure type
  virtual bool IsStruct() const = 0;
  // return true if is enumeration type
  virtual bool IsEnum() const = 0;
  // return true if is constant type
  virtual bool IsConst() const = 0;
  // return true if is function type
  virtual bool IsFunction() const = 0;
  // return true if is volatiled type
  virtual bool IsVola() const = 0;
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
  // return true if two types are identical
  // (ignore left/right value, const, volatile and reference)
  virtual bool IsIdentical(const TypePtr &type) const = 0;
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
  virtual TypePtr GetElem(std::size_t index) const = 0;
  // return the element with specific name
  virtual TypePtr GetElem(const std::string &name) const = 0;
  // return the dereferenced type of current type
  virtual TypePtr GetDerefedType() const = 0;
  // return the deconsted type of current type
  virtual TypePtr GetDeconstedType() const = 0;
  // return the identifier of current type
  virtual std::string GetTypeId() const = 0;
  // return a new type with specific value type (left/right)
  virtual TypePtr GetValueType(bool is_right) const = 0;
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

  bool IsRightValue() const override { return is_right_; }
  bool IsVoid() const override { return type_ == Type::Void; }
  bool IsNull() const override { return type_ == Type::Null; }
  bool IsBasic() const override {
    return type_ != Type::Void && type_ != Type::Null;
  }
  bool IsInteger() const override {
    auto t = static_cast<int>(type_);
    return t >= static_cast<int>(Type::Int8) &&
           t <= static_cast<int>(Type::UInt64);
  }
  bool IsUnsigned() const override {
    auto t = static_cast<int>(type_);
    return t >= static_cast<int>(Type::UInt8) &&
           t <= static_cast<int>(Type::UInt64);
  }
  bool IsFloat() const override {
    return type_ == Type::Float32 || type_ == Type::Float64;
  }
  bool IsBool() const override { return type_ == Type::Bool; }
  bool IsStruct() const override { return false; }
  bool IsEnum() const override { return false; }
  bool IsConst() const override { return false; }
  bool IsFunction() const override { return false; }
  bool IsVola() const override { return false; }
  bool IsArray() const override { return false; }
  bool IsPointer() const override { return false; }
  bool IsReference() const override { return false; }
  std::optional<TypePtrList> GetArgsType() const override { return {}; }
  TypePtr GetReturnType(const TypePtrList &args) const override {
    return nullptr;
  }
  std::size_t GetLength() const override { return 0; }
  TypePtr GetElem(std::size_t index) const override { return nullptr; }
  TypePtr GetElem(const std::string &name) const override {
    return nullptr;
  }
  TypePtr GetDerefedType() const override { return nullptr; }
  TypePtr GetDeconstedType() const override { return nullptr; }

  bool CanAccept(const TypePtr &type) const override;
  bool CanCastTo(const TypePtr &type) const override;
  bool IsIdentical(const TypePtr &type) const override;
  std::size_t GetSize() const override;
  std::string GetTypeId() const override;
  TypePtr GetValueType(bool is_right) const override;

 private:
  Type type_;
  bool is_right_;
};

class StructType : public BaseType {
 public:
  StructType(TypePairList elems, const std::string &id, bool is_right)
      : elems_(std::move(elems)), id_(id), is_right_(is_right) {
    CalcSize();
  }

  bool IsRightValue() const override { return is_right_; }
  bool IsVoid() const override { return false; }
  bool IsNull() const override { return false; }
  bool IsBasic() const override { return false; }
  bool IsInteger() const override { return false; }
  bool IsUnsigned() const override { return false; }
  bool IsFloat() const override { return false; }
  bool IsBool() const override { return false; }
  bool IsStruct() const override { return true; }
  bool IsEnum() const override { return false; }
  bool IsConst() const override { return false; }
  bool IsFunction() const override { return false; }
  bool IsVola() const override { return false; }
  bool IsArray() const override { return false; }
  bool IsPointer() const override { return false; }
  bool IsReference() const override { return false; }
  bool CanCastTo(const TypePtr &type) const override {
    return IsIdentical(type);
  }
  std::size_t GetSize() const override { return size_; }
  std::optional<TypePtrList> GetArgsType() const override { return {}; }
  TypePtr GetReturnType(const TypePtrList &args) const override {
    return nullptr;
  }
  std::size_t GetLength() const override { return elems_.size(); }
  TypePtr GetElem(std::size_t index) const override {
    return elems_[index].second;
  }
  TypePtr GetDerefedType() const override { return nullptr; }
  TypePtr GetDeconstedType() const override { return nullptr; }
  std::string GetTypeId() const override { return id_; }

  bool CanAccept(const TypePtr &type) const override;
  bool IsIdentical(const TypePtr &type) const override;
  TypePtr GetElem(const std::string &name) const override;
  TypePtr GetValueType(bool is_right) const override;

  // setters
  void set_elems(TypePairList elems) { elems_ = std::move(elems); }

 private:
  void CalcSize();

  TypePairList elems_;
  std::string id_;
  bool is_right_;
  std::size_t size_;
};

class EnumType : public BaseType {
 public:
  using ElemSet = std::unordered_set<std::string>;

  EnumType(TypePtr type, ElemSet elems, const std::string &id,
           bool is_right)
      : type_(std::move(type)), elems_(std::move(elems)),
        id_(id), is_right_(is_right) {}

  bool IsRightValue() const override { return is_right_; }
  bool IsVoid() const override { return false; }
  bool IsNull() const override { return false; }
  bool IsBasic() const override { return false; }
  bool IsInteger() const override { return false; }
  bool IsUnsigned() const override { return type_->IsUnsigned(); }
  bool IsFloat() const override { return false; }
  bool IsBool() const override { return false; }
  bool IsStruct() const override { return false; }
  bool IsEnum() const override { return true; }
  bool IsConst() const override { return false; }
  bool IsFunction() const override { return false; }
  bool IsVola() const override { return false; }
  bool IsArray() const override { return false; }
  bool IsPointer() const override { return false; }
  bool IsReference() const override { return false; }
  bool CanCastTo(const TypePtr &type) const override {
    return type_->CanCastTo(type);
  }
  std::size_t GetSize() const override { return type_->GetSize(); }
  std::optional<TypePtrList> GetArgsType() const override { return {}; }
  TypePtr GetReturnType(const TypePtrList &args) const override {
    return nullptr;
  }
  std::size_t GetLength() const override { return 0; }
  TypePtr GetElem(std::size_t index) const override { return nullptr; }
  TypePtr GetDerefedType() const override { return nullptr; }
  TypePtr GetDeconstedType() const override { return nullptr; }
  std::string GetTypeId() const override { return id_; }

  bool CanAccept(const TypePtr &type) const override;
  bool IsIdentical(const TypePtr &type) const override;
  TypePtr GetElem(const std::string &name) const override;
  TypePtr GetValueType(bool is_right) const override;

 private:
  TypePtr type_;
  ElemSet elems_;
  std::string id_;
  bool is_right_;
};

class ConstType : public BaseType {
 public:
  ConstType(TypePtr type) : type_(type) {}

  bool IsRightValue() const override { return type_->IsRightValue(); }
  bool IsVoid() const override { return type_->IsVoid(); }
  bool IsNull() const override { return type_->IsNull(); }
  bool IsBasic() const override { return type_->IsBasic(); }
  bool IsInteger() const override { return type_->IsInteger(); }
  bool IsUnsigned() const override { return type_->IsUnsigned(); }
  bool IsFloat() const override { return type_->IsFloat(); }
  bool IsBool() const override { return type_->IsBool(); }
  bool IsStruct() const override { return type_->IsStruct(); }
  bool IsEnum() const override { return type_->IsEnum(); }
  bool IsConst() const override { return true; }
  bool IsFunction() const override { return type_->IsFunction(); }
  bool IsVola() const override { return type_->IsVola(); }
  bool IsArray() const override { return type_->IsArray(); }
  bool IsPointer() const override { return type_->IsPointer(); }
  bool IsReference() const override { return type_->IsReference(); }
  bool CanAccept(const TypePtr &type) const override {
    return type_->IsReference() ? type_->CanAccept(type) : false;
  }
  bool CanCastTo(const TypePtr &type) const override {
    return type->IsConst() && type_->CanCastTo(type);
  }
  bool IsIdentical(const TypePtr &type) const override {
    return type_->IsIdentical(type);
  }
  std::size_t GetSize() const override { return type_->GetSize(); }
  std::optional<TypePtrList> GetArgsType() const override {
    return type_->GetArgsType();
  }
  TypePtr GetReturnType(const TypePtrList &args) const override {
    return type_->GetReturnType(args);
  }
  std::size_t GetLength() const override { return type_->GetLength(); }
  TypePtr GetDerefedType() const override {
    return type_->GetDerefedType();
  }
  TypePtr GetDeconstedType() const override { return type_; }
  std::string GetTypeId() const override {
    return type_->GetTypeId();
  }

  TypePtr GetElem(std::size_t index) const override;
  TypePtr GetElem(const std::string &name) const override;
  TypePtr GetValueType(bool is_right) const override;

 private:
  TypePtr type_;
};

class FuncType : public BaseType {
 public:
  FuncType(TypePtrList args, TypePtr ret, bool is_right)
      : args_(std::move(args)), ret_(std::move(ret)),
        is_right_(is_right) {}

  bool IsRightValue() const override { return is_right_; }
  bool IsVoid() const override { return false; }
  bool IsNull() const override { return false; }
  bool IsBasic() const override { return true; }
  bool IsInteger() const override { return false; }
  bool IsUnsigned() const override { return false; }
  bool IsFloat() const override { return false; }
  bool IsBool() const override { return false; }
  bool IsStruct() const override { return false; }
  bool IsEnum() const override { return false; }
  bool IsConst() const override { return false; }
  bool IsFunction() const override { return true; }
  bool IsVola() const override { return false; }
  bool IsArray() const override { return false; }
  bool IsPointer() const override { return false; }
  bool IsReference() const override { return false; }
  std::optional<TypePtrList> GetArgsType() const override { return args_; }
  std::size_t GetLength() const override { return 0; }
  TypePtr GetElem(std::size_t index) const override { return nullptr; }
  TypePtr GetElem(const std::string &name) const override {
    return nullptr;
  }
  TypePtr GetDerefedType() const override { return nullptr; }
  TypePtr GetDeconstedType() const override { return nullptr; }

  bool CanAccept(const TypePtr &type) const override;
  bool CanCastTo(const TypePtr &type) const override;
  bool IsIdentical(const TypePtr &type) const override;
  std::size_t GetSize() const override;
  TypePtr GetReturnType(const TypePtrList &args) const override;
  std::string GetTypeId() const override;
  TypePtr GetValueType(bool is_right) const override;

 private:
  TypePtrList args_;
  TypePtr ret_;
  bool is_right_;
};

class VolaType : public BaseType {
 public:
  VolaType(TypePtr type) : type_(std::move(type)) {}

  bool IsRightValue() const override { return type_->IsRightValue(); }
  bool IsVoid() const override { return type_->IsVoid(); }
  bool IsNull() const override { return type_->IsNull(); }
  bool IsBasic() const override { return type_->IsBasic(); }
  bool IsInteger() const override { return type_->IsInteger(); }
  bool IsUnsigned() const override { return type_->IsUnsigned(); }
  bool IsFloat() const override { return type_->IsFloat(); }
  bool IsBool() const override { return type_->IsBool(); }
  bool IsStruct() const override { return type_->IsStruct(); }
  bool IsEnum() const override { return type_->IsEnum(); }
  bool IsConst() const override { return type_->IsConst(); }
  bool IsFunction() const override { return type_->IsFunction(); }
  bool IsVola() const override { return true; }
  bool IsArray() const override { return type_->IsArray(); }
  bool IsPointer() const override { return type_->IsPointer(); }
  bool IsReference() const override { return type_->IsReference(); }
  bool CanAccept(const TypePtr &type) const override {
    return type_->CanAccept(type);
  }
  bool CanCastTo(const TypePtr &type) const override {
    return type_->CanCastTo(type);
  }
  bool IsIdentical(const TypePtr &type) const override {
    return type_->IsIdentical(type);
  }
  std::size_t GetSize() const override { return type_->GetSize(); }
  std::optional<TypePtrList> GetArgsType() const override {
    return type_->GetArgsType();
  }
  TypePtr GetReturnType(const TypePtrList &args) const override {
    return type_->GetReturnType(args);
  }
  std::size_t GetLength() const override { return type_->GetLength(); }
  TypePtr GetElem(std::size_t index) const override {
    return type_->GetElem(index);
  }
  TypePtr GetElem(const std::string &name) const override {
    return type_->GetElem(name);
  }
  TypePtr GetDerefedType() const override {
    return type_->GetDerefedType();
  }
  std::string GetTypeId() const override { return type_->GetTypeId(); }

  TypePtr GetDeconstedType() const override;
  TypePtr GetValueType(bool is_right) const override;

 private:
  TypePtr type_;
};

class ArrayType : public BaseType {
 public:
  ArrayType(TypePtr base, std::size_t len, bool is_right)
      : base_(std::move(base)), len_(len), is_right_(is_right) {}

  bool IsRightValue() const override { return is_right_; }
  bool IsVoid() const override { return false; }
  bool IsNull() const override { return false; }
  bool IsBasic() const override { return false; }
  bool IsInteger() const override { return false; }
  bool IsUnsigned() const override { return false; }
  bool IsFloat() const override { return false; }
  bool IsBool() const override { return false; }
  bool IsStruct() const override { return false; }
  bool IsEnum() const override { return false; }
  bool IsConst() const override { return false; }
  bool IsFunction() const override { return false; }
  bool IsVola() const override { return false; }
  bool IsArray() const override { return true; }
  bool IsPointer() const override { return false; }
  bool IsReference() const override { return false; }
  std::size_t GetSize() const override {
    return base_->GetSize() * len_;
  }
  std::optional<TypePtrList> GetArgsType() const override { return {}; }
  TypePtr GetReturnType(const TypePtrList &args) const override {
    return nullptr;
  }
  std::size_t GetLength() const override { return len_; }
  TypePtr GetElem(std::size_t index) const override { return base_; }
  TypePtr GetElem(const std::string &name) const override {
    return nullptr;
  }
  TypePtr GetDerefedType() const override { return base_; }
  TypePtr GetDeconstedType() const override { return nullptr; }

  bool CanAccept(const TypePtr &type) const override;
  bool CanCastTo(const TypePtr &type) const override;
  bool IsIdentical(const TypePtr &type) const override;
  std::string GetTypeId() const override;
  TypePtr GetValueType(bool is_right) const override;

 private:
  TypePtr base_;
  std::size_t len_;
  bool is_right_;
};

class PointerType : public BaseType {
 public:
  PointerType(TypePtr base, bool is_right)
      : base_(std::move(base)), is_right_(is_right) {}

  bool IsRightValue() const override { return is_right_; }
  bool IsVoid() const override { return false; }
  bool IsNull() const override { return false; }
  bool IsBasic() const override { return true; }
  bool IsInteger() const override { return false; }
  bool IsUnsigned() const override { return false; }
  bool IsFloat() const override { return false; }
  bool IsBool() const override { return false; }
  bool IsStruct() const override { return false; }
  bool IsEnum() const override { return false; }
  bool IsConst() const override { return false; }
  bool IsFunction() const override { return false; }
  bool IsVola() const override { return false; }
  bool IsArray() const override { return false; }
  bool IsPointer() const override { return true; }
  bool IsReference() const override { return false; }
  std::optional<TypePtrList> GetArgsType() const override { return {}; }
  TypePtr GetReturnType(const TypePtrList &args) const override {
    return nullptr;
  }
  std::size_t GetLength() const override { return 0; }
  TypePtr GetElem(std::size_t index) const override { return nullptr; }
  TypePtr GetElem(const std::string &name) const override {
    return nullptr;
  }
  TypePtr GetDerefedType() const override { return base_; }
  TypePtr GetDeconstedType() const override { return nullptr; }

  bool CanAccept(const TypePtr &type) const override;
  bool CanCastTo(const TypePtr &type) const override;
  bool IsIdentical(const TypePtr &type) const override;
  std::size_t GetSize() const override;
  std::string GetTypeId() const override;
  TypePtr GetValueType(bool is_right) const override;

 private:
  TypePtr base_;
  bool is_right_;
};

class RefType : public BaseType {
 public:
  RefType(TypePtr base) : base_(std::move(base)) {}

  bool IsRightValue() const override { return base_->IsRightValue(); }
  bool IsVoid() const override { return base_->IsVoid(); }
  bool IsNull() const override { return base_->IsNull(); }
  bool IsBasic() const override { return base_->IsBasic(); }
  bool IsInteger() const override { return base_->IsInteger(); }
  bool IsUnsigned() const override { return base_->IsUnsigned(); }
  bool IsFloat() const override { return base_->IsFloat(); }
  bool IsBool() const override { return base_->IsBool(); }
  bool IsStruct() const override { return base_->IsStruct(); }
  bool IsEnum() const override { return base_->IsEnum(); }
  bool IsConst() const override { return base_->IsConst(); }
  bool IsFunction() const override { return base_->IsFunction(); }
  bool IsVola() const override { return base_->IsVola(); }
  bool IsArray() const override { return base_->IsArray(); }
  bool IsPointer() const override { return base_->IsPointer(); }
  bool IsReference() const override { return true; }
  bool CanAccept(const TypePtr &type) const override {
    return base_->CanAccept(type);
  }
  bool CanCastTo(const TypePtr &type) const override {
    return base_->CanCastTo(type);
  }
  bool IsIdentical(const TypePtr &type) const override {
    return base_->IsIdentical(type);
  }
  std::size_t GetSize() const override {
    base_->GetSize();
  }
  std::optional<TypePtrList> GetArgsType() const override {
    return base_->GetArgsType();
  }
  TypePtr GetReturnType(const TypePtrList &args) const override {
    return base_->GetReturnType(args);
  }
  std::size_t GetLength() const override { return base_->GetLength(); }
  TypePtr GetElem(std::size_t index) const override {
    return base_->GetElem(index);
  }
  TypePtr GetElem(const std::string &name) const override {
    return base_->GetElem(name);
  }
  TypePtr GetDerefedType() const override {
    return base_;
  }
  std::string GetTypeId() const override {
    return base_->GetTypeId();
  }

  TypePtr GetDeconstedType() const override;
  TypePtr GetValueType(bool is_right) const override;

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
  return std::make_shared<PrimType>(PrimType::Type::Void, true);
}

// set the size of pointers
void SetPointerSize(std::size_t size);

}  // namespace yulang::define

#endif  // YULANG_DEFINE_TYPE_H_
