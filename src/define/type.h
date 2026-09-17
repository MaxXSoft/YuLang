#ifndef YULANG_DEFINE_TYPE_H_
#define YULANG_DEFINE_TYPE_H_

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

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
  BaseType() = default;
  BaseType(const BaseType &) = default;
  BaseType &operator=(const BaseType &) = default;
  BaseType(BaseType &&) = default;
  BaseType &operator=(BaseType &&) = default;

  virtual ~BaseType() = default;

  // return true if is right value
  [[nodiscard]] virtual bool IsRightValue() const = 0;
  // return true if is void type
  [[nodiscard]] virtual bool IsVoid() const = 0;
  // return true if is null type
  [[nodiscard]] virtual bool IsNull() const = 0;
  // return true if is basic type
  // i.e. is primitive type, function, pointer, or their reference
  [[nodiscard]] virtual bool IsBasic() const = 0;
  // return true if is integer
  [[nodiscard]] virtual bool IsInteger() const = 0;
  // return true if is unsigned
  [[nodiscard]] virtual bool IsUnsigned() const = 0;
  // return true if is floating point
  [[nodiscard]] virtual bool IsFloat() const = 0;
  // return true if is boolean
  [[nodiscard]] virtual bool IsBool() const = 0;
  // return true if is structure type
  [[nodiscard]] virtual bool IsStruct() const = 0;
  // return true if is enumeration type
  [[nodiscard]] virtual bool IsEnum() const = 0;
  // return true if is constant type
  [[nodiscard]] virtual bool IsConst() const = 0;
  // return true if is function type
  [[nodiscard]] virtual bool IsFunction() const = 0;
  // return true if is volatiled type
  [[nodiscard]] virtual bool IsVola() const = 0;
  // return true if is array type
  [[nodiscard]] virtual bool IsArray() const = 0;
  // return true if is pointer type
  [[nodiscard]] virtual bool IsPointer() const = 0;
  // return true if is reference type
  [[nodiscard]] virtual bool IsReference() const = 0;
  // return true if left value which is current type
  // can accept the right value which is specific type
  [[nodiscard]] virtual bool CanAccept(const TypePtr &type) const = 0;
  // return true if current type can be casted to specific type
  [[nodiscard]] virtual bool CanCastTo(const TypePtr &type) const = 0;
  // return true if two types are identical
  // (ignore left/right value, const, volatile and reference)
  [[nodiscard]] virtual bool IsIdentical(const TypePtr &type) const = 0;
  // return the size of current type
  [[nodiscard]] virtual std::size_t GetSize() const = 0;
  // return the alignment size of current type
  [[nodiscard]] virtual std::size_t GetAlignSize() const = 0;
  // return the type of arguments of a function call
  [[nodiscard]] virtual std::optional<TypePtrList> GetArgsType() const = 0;
  // return the return type of a function call
  [[nodiscard]] virtual TypePtr GetReturnType(
      const TypePtrList &args) const = 0;
  // return the length of current type
  // e.g. array length, struct field number
  [[nodiscard]] virtual std::size_t GetLength() const = 0;
  // return the element at specific index
  [[nodiscard]] virtual TypePtr GetElem(std::size_t index) const = 0;
  // return the element with specific name
  [[nodiscard]] virtual TypePtr GetElem(const std::string &name) const = 0;
  // return the index of element with specific name
  [[nodiscard]] virtual std::optional<std::size_t> GetElemIndex(
      const std::string &name) const = 0;
  // return the dereferenced type of current type
  [[nodiscard]] virtual TypePtr GetDerefedType() const = 0;
  // return the deconsted type of current type
  [[nodiscard]] virtual TypePtr GetDeconstedType() const = 0;
  // return the identifier of current type
  [[nodiscard]] virtual std::string GetTypeId() const = 0;
  // return a new type with specific value type (left/right)
  [[nodiscard]] virtual TypePtr GetValueType(bool is_right) const = 0;
  // return a new trivial type
  // i.e. all left values, no constants,
  //      replace enumerations with integers,
  //      replace references with pointers
  [[nodiscard]] virtual TypePtr GetTrivialType() const = 0;

  // setters
  static void set_ptr_size(std::size_t ptr_size) { ptr_size_ = ptr_size; }

  // getters
  static std::size_t ptr_size() { return ptr_size_; }

 private:
  // size of pointer
  static std::size_t ptr_size_;
};

class PrimType : public BaseType {
 public:
  enum class Type : std::uint8_t {
    Void,
    Null,
    Int8,
    Int16,
    Int32,
    Int64,
    ISize,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    USize,
    Bool,
    Float32,
    Float64,
  };

  PrimType(Type type, bool is_right) : type_(type), is_right_(is_right) {}

  [[nodiscard]] bool IsRightValue() const override { return is_right_; }
  [[nodiscard]] bool IsVoid() const override { return type_ == Type::Void; }
  [[nodiscard]] bool IsNull() const override { return type_ == Type::Null; }
  [[nodiscard]] bool IsBasic() const override {
    return type_ != Type::Void && type_ != Type::Null;
  }
  [[nodiscard]] bool IsInteger() const override {
    const auto t = static_cast<int>(type_);
    return t >= static_cast<int>(Type::Int8) &&
           t <= static_cast<int>(Type::USize);
  }
  [[nodiscard]] bool IsUnsigned() const override {
    const auto t = static_cast<int>(type_);
    return t >= static_cast<int>(Type::UInt8) &&
           t <= static_cast<int>(Type::USize);
  }
  [[nodiscard]] bool IsFloat() const override {
    return type_ == Type::Float32 || type_ == Type::Float64;
  }
  [[nodiscard]] bool IsBool() const override { return type_ == Type::Bool; }
  [[nodiscard]] bool IsStruct() const override { return false; }
  [[nodiscard]] bool IsEnum() const override { return false; }
  [[nodiscard]] bool IsConst() const override { return false; }
  [[nodiscard]] bool IsFunction() const override { return false; }
  [[nodiscard]] bool IsVola() const override { return false; }
  [[nodiscard]] bool IsArray() const override { return false; }
  [[nodiscard]] bool IsPointer() const override { return false; }
  [[nodiscard]] bool IsReference() const override { return false; }
  [[nodiscard]] std::size_t GetAlignSize() const override { return GetSize(); }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return {};
  }
  [[nodiscard]] TypePtr GetReturnType(
      const TypePtrList & /*args*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::size_t GetLength() const override { return 0; }
  [[nodiscard]] TypePtr GetElem(std::size_t /*index*/) const override {
    return nullptr;
  }
  [[nodiscard]] TypePtr GetElem(const std::string & /*name*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string & /*name*/) const override {
    return {};
  }
  [[nodiscard]] TypePtr GetDerefedType() const override { return nullptr; }
  [[nodiscard]] TypePtr GetDeconstedType() const override { return nullptr; }
  [[nodiscard]] TypePtr GetTrivialType() const override {
    return GetValueType(false);
  }

  [[nodiscard]] bool CanAccept(const TypePtr &type) const override;
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override;
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override;
  [[nodiscard]] std::size_t GetSize() const override;
  [[nodiscard]] std::string GetTypeId() const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;

 private:
  Type type_;
  bool is_right_;
};

class StructType : public BaseType {
 public:
  StructType(TypePairList elems, std::string id, bool is_right)
      : elems_(std::move(elems)), id_(std::move(id)), is_right_(is_right) {
    CalcSize();
  }

  [[nodiscard]] bool IsRightValue() const override { return is_right_; }
  [[nodiscard]] bool IsVoid() const override { return false; }
  [[nodiscard]] bool IsNull() const override { return false; }
  [[nodiscard]] bool IsBasic() const override { return false; }
  [[nodiscard]] bool IsInteger() const override { return false; }
  [[nodiscard]] bool IsUnsigned() const override { return false; }
  [[nodiscard]] bool IsFloat() const override { return false; }
  [[nodiscard]] bool IsBool() const override { return false; }
  [[nodiscard]] bool IsStruct() const override { return true; }
  [[nodiscard]] bool IsEnum() const override { return false; }
  [[nodiscard]] bool IsConst() const override { return false; }
  [[nodiscard]] bool IsFunction() const override { return false; }
  [[nodiscard]] bool IsVola() const override { return false; }
  [[nodiscard]] bool IsArray() const override { return false; }
  [[nodiscard]] bool IsPointer() const override { return false; }
  [[nodiscard]] bool IsReference() const override { return false; }
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override {
    return IsIdentical(type);
  }
  [[nodiscard]] std::size_t GetSize() const override { return size_; }
  [[nodiscard]] std::size_t GetAlignSize() const override { return base_size_; }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return {};
  }
  [[nodiscard]] TypePtr GetReturnType(
      const TypePtrList & /*args*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::size_t GetLength() const override { return elems_.size(); }
  [[nodiscard]] TypePtr GetElem(std::size_t index) const override {
    return elems_[index].second;
  }
  [[nodiscard]] TypePtr GetDerefedType() const override { return nullptr; }
  [[nodiscard]] TypePtr GetDeconstedType() const override { return nullptr; }
  [[nodiscard]] std::string GetTypeId() const override { return id_; }

  [[nodiscard]] bool CanAccept(const TypePtr &type) const override;
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override;
  [[nodiscard]] TypePtr GetElem(const std::string &name) const override;
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string &name) const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;
  [[nodiscard]] TypePtr GetTrivialType() const override;

  // setters
  void set_elems(TypePairList elems) {
    elems_ = std::move(elems);
    CalcSize();
  }

 private:
  void CalcSize();

  TypePairList elems_;
  std::string id_;
  bool is_right_;
  std::size_t size_{}, base_size_{};
};

class EnumType : public BaseType {
 public:
  using ElemSet = std::unordered_set<std::string>;

  EnumType(TypePtr type, ElemSet elems, std::string id, bool is_right)
      : type_(std::move(type)),
        elems_(std::move(elems)),
        id_(std::move(id)),
        is_right_(is_right) {}

  [[nodiscard]] bool IsRightValue() const override { return is_right_; }
  [[nodiscard]] bool IsVoid() const override { return false; }
  [[nodiscard]] bool IsNull() const override { return false; }
  [[nodiscard]] bool IsBasic() const override { return false; }
  [[nodiscard]] bool IsInteger() const override { return false; }
  [[nodiscard]] bool IsUnsigned() const override { return type_->IsUnsigned(); }
  [[nodiscard]] bool IsFloat() const override { return false; }
  [[nodiscard]] bool IsBool() const override { return false; }
  [[nodiscard]] bool IsStruct() const override { return false; }
  [[nodiscard]] bool IsEnum() const override { return true; }
  [[nodiscard]] bool IsConst() const override { return false; }
  [[nodiscard]] bool IsFunction() const override { return false; }
  [[nodiscard]] bool IsVola() const override { return false; }
  [[nodiscard]] bool IsArray() const override { return false; }
  [[nodiscard]] bool IsPointer() const override { return false; }
  [[nodiscard]] bool IsReference() const override { return false; }
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override {
    return type_->CanCastTo(type);
  }
  [[nodiscard]] std::size_t GetSize() const override {
    return type_->GetSize();
  }
  [[nodiscard]] std::size_t GetAlignSize() const override {
    return type_->GetAlignSize();
  }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return {};
  }
  [[nodiscard]] TypePtr GetReturnType(
      const TypePtrList & /*args*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::size_t GetLength() const override { return 0; }
  [[nodiscard]] TypePtr GetElem(std::size_t /*index*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string & /*name*/) const override {
    return {};
  }
  [[nodiscard]] TypePtr GetDerefedType() const override { return nullptr; }
  [[nodiscard]] TypePtr GetDeconstedType() const override { return nullptr; }
  [[nodiscard]] std::string GetTypeId() const override { return id_; }
  [[nodiscard]] TypePtr GetTrivialType() const override {
    return type_->GetTrivialType();
  }

  [[nodiscard]] bool CanAccept(const TypePtr &type) const override;
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override;
  [[nodiscard]] TypePtr GetElem(const std::string &name) const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;

 private:
  TypePtr type_;
  ElemSet elems_;
  std::string id_;
  bool is_right_;
};

class ConstType : public BaseType {
 public:
  explicit ConstType(TypePtr type) : type_(std::move(type)) {}

  [[nodiscard]] bool IsRightValue() const override {
    return type_->IsRightValue();
  }
  [[nodiscard]] bool IsVoid() const override { return type_->IsVoid(); }
  [[nodiscard]] bool IsNull() const override { return type_->IsNull(); }
  [[nodiscard]] bool IsBasic() const override { return type_->IsBasic(); }
  [[nodiscard]] bool IsInteger() const override { return type_->IsInteger(); }
  [[nodiscard]] bool IsUnsigned() const override { return type_->IsUnsigned(); }
  [[nodiscard]] bool IsFloat() const override { return type_->IsFloat(); }
  [[nodiscard]] bool IsBool() const override { return type_->IsBool(); }
  [[nodiscard]] bool IsStruct() const override { return type_->IsStruct(); }
  [[nodiscard]] bool IsEnum() const override { return type_->IsEnum(); }
  [[nodiscard]] bool IsConst() const override { return true; }
  [[nodiscard]] bool IsFunction() const override { return type_->IsFunction(); }
  [[nodiscard]] bool IsVola() const override { return type_->IsVola(); }
  [[nodiscard]] bool IsArray() const override { return type_->IsArray(); }
  [[nodiscard]] bool IsPointer() const override { return type_->IsPointer(); }
  [[nodiscard]] bool IsReference() const override {
    return type_->IsReference();
  }
  [[nodiscard]] bool CanAccept(const TypePtr &type) const override {
    return type_->IsReference() ? type_->CanAccept(type) : false;
  }
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override {
    return type_->CanCastTo(type->IsConst() ? type->GetDeconstedType() : type);
  }
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override {
    return type_->IsIdentical(type->IsConst() ? type->GetDeconstedType()
                                              : type);
  }
  [[nodiscard]] std::size_t GetSize() const override {
    return type_->GetSize();
  }
  [[nodiscard]] std::size_t GetAlignSize() const override {
    return type_->GetAlignSize();
  }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return type_->GetArgsType();
  }
  [[nodiscard]] TypePtr GetReturnType(const TypePtrList &args) const override {
    return type_->GetReturnType(args);
  }
  [[nodiscard]] std::size_t GetLength() const override {
    return type_->GetLength();
  }
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string &name) const override {
    return type_->GetElemIndex(name);
  }
  [[nodiscard]] TypePtr GetDerefedType() const override {
    return type_->GetDerefedType();
  }
  [[nodiscard]] TypePtr GetDeconstedType() const override { return type_; }
  [[nodiscard]] std::string GetTypeId() const override {
    return type_->GetTypeId();
  }
  [[nodiscard]] TypePtr GetTrivialType() const override {
    return type_->GetTrivialType();
  }

  [[nodiscard]] TypePtr GetElem(std::size_t index) const override;
  [[nodiscard]] TypePtr GetElem(const std::string &name) const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;

 private:
  TypePtr type_;
};

class FuncType : public BaseType {
 public:
  FuncType(TypePtrList args, TypePtr ret, bool is_right)
      : args_(std::move(args)), ret_(std::move(ret)), is_right_(is_right) {}

  [[nodiscard]] bool IsRightValue() const override { return is_right_; }
  [[nodiscard]] bool IsVoid() const override { return false; }
  [[nodiscard]] bool IsNull() const override { return false; }
  [[nodiscard]] bool IsBasic() const override { return true; }
  [[nodiscard]] bool IsInteger() const override { return false; }
  [[nodiscard]] bool IsUnsigned() const override { return false; }
  [[nodiscard]] bool IsFloat() const override { return false; }
  [[nodiscard]] bool IsBool() const override { return false; }
  [[nodiscard]] bool IsStruct() const override { return false; }
  [[nodiscard]] bool IsEnum() const override { return false; }
  [[nodiscard]] bool IsConst() const override { return false; }
  [[nodiscard]] bool IsFunction() const override { return true; }
  [[nodiscard]] bool IsVola() const override { return false; }
  [[nodiscard]] bool IsArray() const override { return false; }
  [[nodiscard]] bool IsPointer() const override { return false; }
  [[nodiscard]] bool IsReference() const override { return false; }
  [[nodiscard]] std::size_t GetAlignSize() const override { return GetSize(); }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return args_;
  }
  [[nodiscard]] std::size_t GetLength() const override { return 0; }
  [[nodiscard]] TypePtr GetElem(std::size_t /*index*/) const override {
    return nullptr;
  }
  [[nodiscard]] TypePtr GetElem(const std::string & /*name*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string & /*name*/) const override {
    return {};
  }
  [[nodiscard]] TypePtr GetDerefedType() const override { return nullptr; }
  [[nodiscard]] TypePtr GetDeconstedType() const override { return nullptr; }

  [[nodiscard]] bool CanAccept(const TypePtr &type) const override;
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override;
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override;
  [[nodiscard]] std::size_t GetSize() const override;
  [[nodiscard]] TypePtr GetReturnType(const TypePtrList &args) const override;
  [[nodiscard]] std::string GetTypeId() const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;
  [[nodiscard]] TypePtr GetTrivialType() const override;

 private:
  TypePtrList args_;
  TypePtr ret_;
  bool is_right_;
};

class VolaType : public BaseType {
 public:
  explicit VolaType(TypePtr type) : type_(std::move(type)) {}

  [[nodiscard]] bool IsRightValue() const override {
    return type_->IsRightValue();
  }
  [[nodiscard]] bool IsVoid() const override { return type_->IsVoid(); }
  [[nodiscard]] bool IsNull() const override { return type_->IsNull(); }
  [[nodiscard]] bool IsBasic() const override { return type_->IsBasic(); }
  [[nodiscard]] bool IsInteger() const override { return type_->IsInteger(); }
  [[nodiscard]] bool IsUnsigned() const override { return type_->IsUnsigned(); }
  [[nodiscard]] bool IsFloat() const override { return type_->IsFloat(); }
  [[nodiscard]] bool IsBool() const override { return type_->IsBool(); }
  [[nodiscard]] bool IsStruct() const override { return type_->IsStruct(); }
  [[nodiscard]] bool IsEnum() const override { return type_->IsEnum(); }
  [[nodiscard]] bool IsConst() const override { return type_->IsConst(); }
  [[nodiscard]] bool IsFunction() const override { return type_->IsFunction(); }
  [[nodiscard]] bool IsVola() const override { return true; }
  [[nodiscard]] bool IsArray() const override { return type_->IsArray(); }
  [[nodiscard]] bool IsPointer() const override { return type_->IsPointer(); }
  [[nodiscard]] bool IsReference() const override {
    return type_->IsReference();
  }
  [[nodiscard]] bool CanAccept(const TypePtr &type) const override {
    return type_->CanAccept(type);
  }
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override {
    return type_->CanCastTo(type);
  }
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override {
    return type_->IsIdentical(type);
  }
  [[nodiscard]] std::size_t GetSize() const override {
    return type_->GetSize();
  }
  [[nodiscard]] std::size_t GetAlignSize() const override {
    return type_->GetAlignSize();
  }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return type_->GetArgsType();
  }
  [[nodiscard]] TypePtr GetReturnType(const TypePtrList &args) const override {
    return type_->GetReturnType(args);
  }
  [[nodiscard]] std::size_t GetLength() const override {
    return type_->GetLength();
  }
  [[nodiscard]] TypePtr GetElem(std::size_t index) const override {
    return type_->GetElem(index);
  }
  [[nodiscard]] TypePtr GetElem(const std::string &name) const override {
    return type_->GetElem(name);
  }
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string &name) const override {
    return type_->GetElemIndex(name);
  }
  [[nodiscard]] TypePtr GetDerefedType() const override {
    return type_->GetDerefedType();
  }
  [[nodiscard]] std::string GetTypeId() const override {
    return type_->GetTypeId();
  }

  [[nodiscard]] TypePtr GetDeconstedType() const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;
  [[nodiscard]] TypePtr GetTrivialType() const override;

 private:
  TypePtr type_;
};

class ArrayType : public BaseType {
 public:
  ArrayType(TypePtr base, std::size_t len, bool is_right)
      : base_(std::move(base)), len_(len), is_right_(is_right) {}

  [[nodiscard]] bool IsRightValue() const override { return is_right_; }
  [[nodiscard]] bool IsVoid() const override { return false; }
  [[nodiscard]] bool IsNull() const override { return false; }
  [[nodiscard]] bool IsBasic() const override { return false; }
  [[nodiscard]] bool IsInteger() const override { return false; }
  [[nodiscard]] bool IsUnsigned() const override { return false; }
  [[nodiscard]] bool IsFloat() const override { return false; }
  [[nodiscard]] bool IsBool() const override { return false; }
  [[nodiscard]] bool IsStruct() const override { return false; }
  [[nodiscard]] bool IsEnum() const override { return false; }
  [[nodiscard]] bool IsConst() const override { return false; }
  [[nodiscard]] bool IsFunction() const override { return false; }
  [[nodiscard]] bool IsVola() const override { return false; }
  [[nodiscard]] bool IsArray() const override { return true; }
  [[nodiscard]] bool IsPointer() const override { return false; }
  [[nodiscard]] bool IsReference() const override { return false; }
  [[nodiscard]] std::size_t GetSize() const override {
    return base_->GetSize() * len_;
  }
  [[nodiscard]] std::size_t GetAlignSize() const override {
    return base_->GetAlignSize();
  }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return {};
  }
  [[nodiscard]] TypePtr GetReturnType(
      const TypePtrList & /*args*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::size_t GetLength() const override { return len_; }
  [[nodiscard]] TypePtr GetElem(std::size_t /*index*/) const override {
    return base_;
  }
  [[nodiscard]] TypePtr GetElem(const std::string & /*name*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string & /*name*/) const override {
    return {};
  }
  [[nodiscard]] TypePtr GetDerefedType() const override { return base_; }
  [[nodiscard]] TypePtr GetDeconstedType() const override { return nullptr; }

  [[nodiscard]] bool CanAccept(const TypePtr &type) const override;
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override;
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override;
  [[nodiscard]] std::string GetTypeId() const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;
  [[nodiscard]] TypePtr GetTrivialType() const override;

 private:
  TypePtr base_;
  std::size_t len_;
  bool is_right_;
};

class PointerType : public BaseType {
 public:
  PointerType(TypePtr base, bool is_right)
      : base_(std::move(base)), is_right_(is_right) {}

  [[nodiscard]] bool IsRightValue() const override { return is_right_; }
  [[nodiscard]] bool IsVoid() const override { return false; }
  [[nodiscard]] bool IsNull() const override { return false; }
  [[nodiscard]] bool IsBasic() const override { return true; }
  [[nodiscard]] bool IsInteger() const override { return false; }
  [[nodiscard]] bool IsUnsigned() const override { return false; }
  [[nodiscard]] bool IsFloat() const override { return false; }
  [[nodiscard]] bool IsBool() const override { return false; }
  [[nodiscard]] bool IsStruct() const override { return false; }
  [[nodiscard]] bool IsEnum() const override { return false; }
  [[nodiscard]] bool IsConst() const override { return false; }
  [[nodiscard]] bool IsFunction() const override { return false; }
  [[nodiscard]] bool IsVola() const override { return false; }
  [[nodiscard]] bool IsArray() const override { return false; }
  [[nodiscard]] bool IsPointer() const override { return true; }
  [[nodiscard]] bool IsReference() const override { return false; }
  [[nodiscard]] std::size_t GetAlignSize() const override { return GetSize(); }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return {};
  }
  [[nodiscard]] TypePtr GetReturnType(
      const TypePtrList & /*args*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::size_t GetLength() const override { return 0; }
  [[nodiscard]] TypePtr GetElem(std::size_t /*index*/) const override {
    return nullptr;
  }
  [[nodiscard]] TypePtr GetElem(const std::string & /*name*/) const override {
    return nullptr;
  }
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string & /*name*/) const override {
    return {};
  }
  [[nodiscard]] TypePtr GetDerefedType() const override { return base_; }
  [[nodiscard]] TypePtr GetDeconstedType() const override { return nullptr; }

  [[nodiscard]] bool CanAccept(const TypePtr &type) const override;
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override;
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override;
  [[nodiscard]] std::size_t GetSize() const override;
  [[nodiscard]] std::string GetTypeId() const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;
  [[nodiscard]] TypePtr GetTrivialType() const override;

 private:
  TypePtr base_;
  bool is_right_;
};

class RefType : public BaseType {
 public:
  explicit RefType(TypePtr base) : base_(std::move(base)) {}

  [[nodiscard]] bool IsRightValue() const override { return false; }
  [[nodiscard]] bool IsVoid() const override { return base_->IsVoid(); }
  [[nodiscard]] bool IsNull() const override { return base_->IsNull(); }
  [[nodiscard]] bool IsBasic() const override { return base_->IsBasic(); }
  [[nodiscard]] bool IsInteger() const override { return base_->IsInteger(); }
  [[nodiscard]] bool IsUnsigned() const override { return base_->IsUnsigned(); }
  [[nodiscard]] bool IsFloat() const override { return base_->IsFloat(); }
  [[nodiscard]] bool IsBool() const override { return base_->IsBool(); }
  [[nodiscard]] bool IsStruct() const override { return base_->IsStruct(); }
  [[nodiscard]] bool IsEnum() const override { return base_->IsEnum(); }
  [[nodiscard]] bool IsConst() const override { return base_->IsConst(); }
  [[nodiscard]] bool IsFunction() const override { return base_->IsFunction(); }
  [[nodiscard]] bool IsVola() const override { return base_->IsVola(); }
  [[nodiscard]] bool IsArray() const override { return base_->IsArray(); }
  [[nodiscard]] bool IsPointer() const override { return base_->IsPointer(); }
  [[nodiscard]] bool IsReference() const override { return true; }
  [[nodiscard]] bool CanAccept(const TypePtr &type) const override {
    return base_->CanAccept(type->IsReference() ? type->GetDerefedType()
                                                : type);
  }
  [[nodiscard]] bool CanCastTo(const TypePtr &type) const override {
    return base_->CanCastTo(type->IsReference() ? type->GetDerefedType()
                                                : type);
  }
  [[nodiscard]] bool IsIdentical(const TypePtr &type) const override {
    return base_->IsIdentical(type->IsReference() ? type->GetDerefedType()
                                                  : type);
  }
  [[nodiscard]] std::size_t GetSize() const override {
    return base_->GetSize();
  }
  [[nodiscard]] std::size_t GetAlignSize() const override {
    return base_->GetAlignSize();
  }
  [[nodiscard]] std::optional<TypePtrList> GetArgsType() const override {
    return base_->GetArgsType();
  }
  [[nodiscard]] TypePtr GetReturnType(const TypePtrList &args) const override {
    return base_->GetReturnType(args);
  }
  [[nodiscard]] std::size_t GetLength() const override {
    return base_->GetLength();
  }
  [[nodiscard]] TypePtr GetElem(std::size_t index) const override {
    return base_->GetElem(index);
  }
  [[nodiscard]] TypePtr GetElem(const std::string &name) const override {
    return base_->GetElem(name);
  }
  [[nodiscard]] std::optional<std::size_t> GetElemIndex(
      const std::string &name) const override {
    return base_->GetElemIndex(name);
  }
  [[nodiscard]] TypePtr GetDerefedType() const override { return base_; }
  [[nodiscard]] std::string GetTypeId() const override {
    return base_->GetTypeId();
  }

  [[nodiscard]] TypePtr GetDeconstedType() const override;
  [[nodiscard]] TypePtr GetValueType(bool is_right) const override;
  [[nodiscard]] TypePtr GetTrivialType() const override;

 private:
  TypePtr base_;
};

// create a new primitive type by keyword
inline TypePtr MakePrimType(Keyword key, bool is_right) {
  using Type = PrimType::Type;
  Type type{};
  switch (key) {
    case Keyword::Null:
      type = Type::Null;
      break;
    case Keyword::Int8:
      type = Type::Int8;
      break;
    case Keyword::Int16:
      type = Type::Int16;
      break;
    case Keyword::Int32:
      type = Type::Int32;
      break;
    case Keyword::Int64:
      type = Type::Int64;
      break;
    case Keyword::ISize:
      type = Type::ISize;
      break;
    case Keyword::UInt8:
      type = Type::UInt8;
      break;
    case Keyword::UInt16:
      type = Type::UInt16;
      break;
    case Keyword::UInt32:
      type = Type::UInt32;
      break;
    case Keyword::UInt64:
      type = Type::UInt64;
      break;
    case Keyword::USize:
      type = Type::USize;
      break;
    case Keyword::Bool:
      type = Type::Bool;
      break;
    case Keyword::Float32:
      type = Type::Float32;
      break;
    case Keyword::Float64:
      type = Type::Float64;
      break;
    default:
      assert(false);
      return nullptr;
  }
  return std::make_shared<PrimType>(type, is_right);
}

// create a new void type
inline TypePtr MakeVoid() {
  return std::make_shared<PrimType>(PrimType::Type::Void, true);
}

// create a new pointer type
inline TypePtr MakePointer(const TypePtr &type, bool is_right) {
  return std::make_shared<PointerType>(type, is_right);
}

// create a new pointer type (right value)
inline TypePtr MakePointer(const TypePtr &type) {
  return std::make_shared<PointerType>(type, true);
}

}  // namespace yulang::define

#endif  // YULANG_DEFINE_TYPE_H_
