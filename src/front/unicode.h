#ifndef YULANG_FRONT_UNICODE_H_
#define YULANG_FRONT_UNICODE_H_

#include <cstddef>
#include <optional>
#include <string_view>

namespace yulang::front::unicode {

// Unicode 17.0.0 XID_Start/XID_Continue, with '_' also allowed at the start.
// Identifier spelling is preserved; no normalization or case folding is done.
bool IsIdentifierStart(char32_t code_point);
bool IsIdentifierContinue(char32_t code_point);
bool IsIdentifier(std::string_view text);

// Zero means an invalid leading byte. DecodeUTF8 expects exactly one scalar
// and rejects overlong encodings, surrogates and values above U+10FFFF.
std::size_t UTF8SequenceLength(unsigned char lead);
std::optional<char32_t> DecodeUTF8(std::string_view bytes);

}  // namespace yulang::front::unicode

#endif  // YULANG_FRONT_UNICODE_H_
