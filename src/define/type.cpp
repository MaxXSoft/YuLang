#include "define/type.h"

#include <sstream>
#include <cassert>

using namespace yulang::define;

namespace {

// size of pointer
// TODO: depends on the backend
std::size_t pointer_size = sizeof(void *);

}  // namespace

bool PrimType::CanAccept(const TypePtr &type) const {
  if (is_right_ || IsNull()) return false;
  return IsIdentical(type);
}

bool PrimType::CanCastTo(const TypePtr &type) const {
  if (IsVoid()) return false;
  return !type->IsReference() && type->IsBasic();
}

bool PrimType::IsIdentical(const TypePtr &type) const {
  if (IsVoid() && type->IsVoid()) return true;
  if (IsNull() && type->IsNull()) return true;
  if (IsInteger() && type->IsInteger()) {
    return IsUnsigned() == type->IsUnsigned() &&
           GetSize() == type->GetSize();
  }
  if (IsFloat() && type->IsFloat()) return GetSize() == type->GetSize();
  if (IsBool() && type->IsBool()) return true;
  return false;
}

std::size_t PrimType::GetSize() const {
  switch (type_) {
    case Type::Bool: return 1;
    case Type::Int8: case Type::UInt8: return 1;
    case Type::Int16: case Type::UInt16: return 2;
    case Type::Int32: case Type::UInt32: case Type::Float32: return 4;
    case Type::Int64: case Type::UInt64: case Type::Float64: return 8;
    case Type::Null: return pointer_size;
    default: return 0;
  }
}

std::string PrimType::GetTypeId() const {
  switch (type_) {
    case Type::Int8: return "i8";
    case Type::Int16: return "i16";
    case Type::Int32: return "i32";
    case Type::Int64: return "i64";
    case Type::UInt8: return "u8";
    case Type::UInt16: return "u16";
    case Type::UInt32: return "u32";
    case Type::UInt64: return "u64";
    case Type::Bool: return "bool";
    case Type::Float32: return "f32";
    case Type::Float64: return "f64";
    default: return "";
  }
}

TypePtr PrimType::GetValueType(bool is_right) const {
  return std::make_shared<PrimType>(type_, is_right);
}

void StructType::CalcSize() {
  std::size_t sum = 0, max_base_size = 1;
  for (const auto &[_, t] : elems_) {
    sum += t->GetSize();
    // update 'max_base_size'
    auto base_size = t->IsArray() ? t->GetDerefedType()->GetSize()
                                  : t->GetSize();
    if (base_size > max_base_size) max_base_size = base_size;
  }
  size_ = (((sum - 1) / max_base_size) + 1) * max_base_size;
}

bool StructType::CanAccept(const TypePtr &type) const {
  return !is_right_ && IsIdentical(type);
}

bool StructType::IsIdentical(const TypePtr &type) const {
  return type->IsStruct() && id_ == type->GetTypeId();
}

TypePtr StructType::GetElem(const std::string &name) const {
  for (const auto &[n, t] : elems_) if (name == n) return t;
  return nullptr;
}

TypePtr StructType::GetValueType(bool is_right) const {
  return std::make_shared<StructType>(elems_, id_, is_right);
}

bool EnumType::CanAccept(const TypePtr &type) const {
  return !is_right_ && IsIdentical(type);
}

bool EnumType::IsIdentical(const TypePtr &type) const {
  return type->IsEnum() && id_ == type->GetTypeId();
}

TypePtr EnumType::GetElem(const std::string &name) const {
  return elems_.find(name) != elems_.end() ? type_ : nullptr;
}

TypePtr EnumType::GetValueType(bool is_right) const {
  return std::make_shared<EnumType>(type_, elems_, id_, is_right);
}

TypePtr ConstType::GetElem(std::size_t index) const {
  auto type = type_->GetElem(index);
  return type_->IsReference()
             ? std::move(type)
             : std::make_shared<ConstType>(std::move(type));
}

TypePtr ConstType::GetElem(const std::string &name) const {
  auto type = type_->GetElem(name);
  if (!type) return nullptr;
  return type_->IsReference()
             ? std::move(type)
             : std::make_shared<ConstType>(std::move(type));
}

TypePtr ConstType::GetValueType(bool is_right) const {
  auto type = type_->GetValueType(is_right);
  return std::make_shared<ConstType>(std::move(type));
}

bool FuncType::CanAccept(const TypePtr &type) const {
  return !is_right_ && (IsIdentical(type) || type->IsNull());
}

bool FuncType::CanCastTo(const TypePtr &type) const {
  return !type->IsReference() && type->IsPointer();
}

bool FuncType::IsIdentical(const TypePtr &type) const {
  if (!type->IsFunction()) return false;
  auto ret = type->GetReturnType(args_);
  return ret ? ret_->IsIdentical(ret) : false;
}

std::size_t FuncType::GetSize() const {
  return pointer_size;
}

TypePtr FuncType::GetReturnType(const TypePtrList &args) const {
  if (args_.size() != args.size()) return nullptr;
  for (int i = 0; i < args_.size(); ++i) {
    if (!args_[i]->IsIdentical(args[i])) return nullptr;
    if (args_[i]->IsReference() && !args[i]->IsReference() &&
        args[i]->IsRightValue()) {
      return nullptr;
    }
  }
  return ret_;
}

std::string FuncType::GetTypeId() const {
  std::ostringstream oss;
  oss << '$' << args_.size() << 'f';
  for (const auto &i : args_) oss << i->GetTypeId();
  oss << '$';
  oss << ret_->GetTypeId();
  return oss.str();
}

TypePtr FuncType::GetValueType(bool is_right) const {
  return std::make_shared<FuncType>(args_, ret_, is_right);
}

TypePtr VolaType::GetDeconstedType() const {
  auto type = type_->GetDeconstedType();
  return std::make_shared<VolaType>(std::move(type));
}

TypePtr VolaType::GetValueType(bool is_right) const {
  auto type = type_->GetValueType(is_right);
  return std::make_shared<VolaType>(std::move(type));
}

bool ArrayType::CanAccept(const TypePtr &type) const {
  return !is_right_ && IsIdentical(type);
}

bool ArrayType::CanCastTo(const TypePtr &type) const {
  return !type->IsReference() && type->IsPointer();
}

bool ArrayType::IsIdentical(const TypePtr &type) const {
  return type->IsArray() && base_->IsIdentical(type->GetDerefedType()) &&
         len_ == type->GetLength();
}

std::string ArrayType::GetTypeId() const {
  std::ostringstream oss;
  oss << '$' << len_ << 'a';
  oss << base_->GetTypeId();
  return oss.str();
}

TypePtr ArrayType::GetValueType(bool is_right) const {
  return std::make_shared<ArrayType>(base_, len_, is_right);
}

bool PointerType::CanAccept(const TypePtr &type) const {
  return !is_right_ && (IsIdentical(type) || type->IsNull());
}

bool PointerType::CanCastTo(const TypePtr &type) const {
  return !type->IsReference() && type->IsBasic();
}

bool PointerType::IsIdentical(const TypePtr &type) const {
  return type->IsPointer() && base_->IsIdentical(type->GetDerefedType());
}

std::size_t PointerType::GetSize() const {
  return pointer_size;
}

std::string PointerType::GetTypeId() const {
  std::ostringstream oss;
  oss << "$p";
  oss << base_->GetTypeId();
  return oss.str();
}

TypePtr PointerType::GetValueType(bool is_right) const {
  return std::make_shared<PointerType>(base_, is_right);
}

TypePtr RefType::GetDeconstedType() const {
  auto type = base_->GetDeconstedType();
  return std::make_shared<RefType>(std::move(type));
}

TypePtr RefType::GetValueType(bool is_right) const {
  auto type = base_->GetValueType(is_right);
  return std::make_shared<RefType>(std::move(type));
}

void SetPointerSize(std::size_t size) {
  pointer_size = size;
}
