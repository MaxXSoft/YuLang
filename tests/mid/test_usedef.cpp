#include <algorithm>
#include <cstdlib>
#include <memory>
#include <new>
#include <utility>

#include "mid/usedef.h"

namespace {
bool fail_allocations = false;
}  // namespace

// Replace allocation only in this test executable to exercise failure paths.
// NOLINTBEGIN(cppcoreguidelines-no-malloc,cppcoreguidelines-owning-memory,readability-inconsistent-declaration-parameter-name)
void *operator new(std::size_t size) {
  if (fail_allocations) throw std::bad_alloc();
  if (void *ptr = std::malloc(size ? size : 1)) return ptr;
  throw std::bad_alloc();
}
void operator delete(void *ptr) noexcept { std::free(ptr); }
// NOLINTEND(cppcoreguidelines-no-malloc,cppcoreguidelines-owning-memory,readability-inconsistent-declaration-parameter-name)

namespace {

class TestValue final : public yulang::mid::User {
 public:
  void Dump(std::ostream & /*os*/,
            yulang::mid::IdManager & /*idm*/) const override {}
  [[nodiscard]] bool IsConst() const override { return false; }
  void RunPass(yulang::mid::PassBase & /*pass*/) override {}
  void GenerateCode(yulang::back::CodeGen & /*pass*/) override {}
};

void Check(bool condition) {
  if (!condition) std::abort();
}

bool Contains(const TestValue &value, const yulang::mid::Use *use) {
  return std::count(value.uses().begin(), value.uses().end(), use) == 1;
}

// Verify the documented empty state of moved-from Use objects.
// NOLINTBEGIN(bugprone-use-after-move,clang-analyzer-cplusplus.Move)
void TestMoves() {
  using yulang::mid::Use;
  const auto first = std::make_shared<TestValue>();
  const auto second = std::make_shared<TestValue>();
  TestValue owner;
  {
    Use source(first, &owner);
    Check(Contains(*first, &source));
    Use moved(std::move(source));
    Check(!source.value() && Contains(*first, &moved));
    Use target(second, &owner);
    target = std::move(moved);
    Check(!moved.value() && second->uses().empty());
    Check(Contains(*first, &target) && first->uses().size() == 1);
    Use same(first, &owner);
    target = std::move(same);
    Check(!same.value() && Contains(*first, &target));
    Check(first->uses().size() == 1);
    auto *alias = &target;
    target = std::move(*alias);
    Check(Contains(*first, &target));
    Use empty(nullptr, &owner);
    empty = std::move(target);
    Check(!target.value() && Contains(*first, &empty));
    empty = std::move(target);
    Check(!empty.value() && first->uses().empty());
  }
  Check(first->uses().empty() && second->uses().empty());
}

void TestAllocationFailure() {
  using yulang::mid::Use;
  const auto first = std::make_shared<TestValue>();
  const auto second = std::make_shared<TestValue>();
  TestValue owner;
  Use source(first, &owner);
  Use target(second, &owner);
  fail_allocations = true;
  Use moved(std::move(source));
  target = std::move(moved);
  fail_allocations = false;
  Check(!source.value() && !moved.value());
  Check(Contains(*first, &target) && second->uses().empty());

  bool caught = false;
  fail_allocations = true;
  try {
    target.set_value(second);
  } catch (const std::bad_alloc &) {
    caught = true;
  }
  fail_allocations = false;
  Check(caught && target.value() == first);
  Check(Contains(*first, &target) && second->uses().empty());
}

// NOLINTEND(bugprone-use-after-move,clang-analyzer-cplusplus.Move)

void TestRelocationAndCopies() {
  using yulang::mid::Use;
  const auto value = std::make_shared<TestValue>();
  {
    TestValue owner;
    owner.AddValue(value);
    owner.AddValue(value);
    owner.Reserve(128);
    Check(value->uses().size() == 2);
    Check(Contains(*value, &owner[0]) && Contains(*value, &owner[1]));
    Use copy(owner[0]);
    Check(Contains(*value, &copy) && value->uses().size() == 3);
    Use assigned(nullptr, &owner);
    assigned = copy;
    Check(Contains(*value, &assigned) && value->uses().size() == 4);
    owner[0].set_value(nullptr);
    owner.RemoveNull();
    Check(owner.size() == 1 && Contains(*value, &owner[0]));
    Check(value->uses().size() == 3);
  }
  Check(value->uses().empty());
}

}  // namespace

int main() try {
  TestMoves();
  TestRelocationAndCopies();
  TestAllocationFailure();
} catch (...) {
  return 1;
}
