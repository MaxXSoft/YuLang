#include "front/unicode.h"

#include <algorithm>
#include <array>

namespace yulang::front::unicode {

namespace {

struct Range {
  char32_t first, last;
};

#include "front/unicode_xid.inc"

template <std::size_t N>
bool Contains(const std::array<Range, N> &ranges, char32_t code_point) {
  const auto it = std::lower_bound(
      ranges.begin(), ranges.end(), code_point,
      [](const Range &range, char32_t value) { return range.last < value; });
  return it != ranges.end() && it->first <= code_point;
}

}  // namespace

bool IsIdentifierStart(char32_t code_point) {
  return code_point == U'_' || Contains(kXIDStart, code_point);
}

bool IsIdentifierContinue(char32_t code_point) {
  return Contains(kXIDContinue, code_point);
}

std::size_t UTF8SequenceLength(unsigned char lead) {
  if (lead <= 0x7f) return 1;
  if (lead >= 0xc2 && lead <= 0xdf) return 2;
  if (lead >= 0xe0 && lead <= 0xef) return 3;
  if (lead >= 0xf0 && lead <= 0xf4) return 4;
  return 0;
}

std::optional<char32_t> DecodeUTF8(std::string_view bytes) {
  if (bytes.empty()) return {};
  const auto lead = static_cast<unsigned char>(bytes.front());
  const auto length = UTF8SequenceLength(lead);
  if (!length || bytes.size() != length) return {};
  if (length == 1) return lead;
  char32_t value = lead & (0x7fU >> length);
  for (std::size_t i = 1; i < length; ++i) {
    const auto byte = static_cast<unsigned char>(bytes[i]);
    if ((byte & 0xc0U) != 0x80U) return {};
    value = (value << 6U) | (byte & 0x3fU);
  }
  constexpr std::array<char32_t, 5> minimum = {0, 0, 0x80, 0x800, 0x10000};
  if (value < minimum[length] || value > 0x10ffff ||
      (value >= 0xd800 && value <= 0xdfff)) {
    return {};
  }
  return value;
}

bool IsIdentifier(std::string_view text) {
  if (text.empty()) return false;
  bool first = true;
  while (!text.empty()) {
    const auto length =
        UTF8SequenceLength(static_cast<unsigned char>(text.front()));
    if (!length || length > text.size()) return false;
    const auto value = DecodeUTF8(text.substr(0, length));
    if (!value ||
        !(first ? IsIdentifierStart(*value) : IsIdentifierContinue(*value))) {
      return false;
    }
    text.remove_prefix(length);
    first = false;
  }
  return true;
}

}  // namespace yulang::front::unicode
