#ifndef YULANG_FRONT_DEPFILE_H_
#define YULANG_FRONT_DEPFILE_H_

#include <filesystem>
#include <string>
#include <vector>

namespace yulang::front {

// Write <output>.d after successful compilation. Paths are not Make-escaped.
bool WriteDepfile(const std::string &output,
                  const std::vector<std::filesystem::path> &dependencies);

}  // namespace yulang::front

#endif  // YULANG_FRONT_DEPFILE_H_
