#include <cstdlib>
#include <memory>

#include "define/type.h"

namespace {

void Check(bool condition) {
  if (!condition) std::abort();
}

}  // namespace

int main() {
  using namespace yulang::define;
  const auto number = std::make_shared<PrimType>(PrimType::Type::Int32, false);
  const auto first =
      std::make_shared<StructType>(TypePairList{}, "Cell", false);
  const auto second =
      std::make_shared<StructType>(TypePairList{}, "Cell", false);
  Check(!first->IsIdentical(second));
  Check(!first->CanCastTo(second));

  // Value-category views made before completion observe the final declaration.
  const auto value = first->GetValueType(true);
  first->SetIdentity("Cell", "first", true);
  first->SetFieldVisibility({false, true});
  first->set_elems({{"hidden", number}, {"visible", number}});
  Check(value->GetLength() == 2 && value->GetSize() == 8);
  Check(first->IsIdentical(value) && value->GetOwnerModule() == "first");
  Check(value->IsPublicType() && !value->IsFieldPublic(0));
  Check(value->IsFieldPublic(1) && !value->IsFieldPublic(2));

  second->SetIdentity("Cell", "second", true);
  second->set_elems({{"hidden", number}, {"visible", number}});
  Check(!first->IsIdentical(second));
  Check(!first->CanAccept(second));
  Check(!first->GetTrivialType()->IsIdentical(second->GetTrivialType()));

  const auto qualified = std::make_shared<RefType>(
      std::make_shared<VolaType>(std::make_shared<ConstType>(first)));
  Check(qualified->IsIdentical(first));
  Check(qualified->GetOwnerModule() == "first");
  Check(qualified->IsPublicType() && qualified->IsFieldPublic(1));
  const auto lowered = qualified->GetTrivialType()->GetDerefedType();
  Check(lowered->IsIdentical(first) && lowered->GetLength() == 2);
  Check(lowered->GetOwnerModule() == "first" && lowered->IsFieldPublic(1));

  const auto pointer = std::make_shared<PointerType>(first, false);
  Check(!pointer->CanAccept(std::make_shared<PointerType>(second, true)));

  const auto enum_a = std::make_shared<EnumType>(number, EnumType::ElemSet{"A"},
                                                 "Choice", false);
  const auto enum_b = std::make_shared<EnumType>(number, EnumType::ElemSet{"A"},
                                                 "Choice", false);
  Check(!enum_a->IsIdentical(enum_b));
  enum_a->SetIdentity("Choice", "first", true);
  enum_b->SetIdentity("Choice", "second", true);
  Check(!enum_a->IsIdentical(enum_b));
  const auto enum_view = enum_a->GetValueType(true);
  Check(enum_a->IsIdentical(enum_view));
  Check(enum_view->GetOwnerModule() == "first" && enum_view->IsPublicType());
}
