#include "front/depfile.h"

#include <fstream>
#include <random>
#include <set>
#include <system_error>

#include "front/logger.h"

namespace yulang::front {

namespace {

// Reserve a directory atomically: C++17 ofstream cannot exclusively create a
// file. Keeping the temporary beside the destination permits atomic rename.
class TemporaryDepfile {
 public:
  TemporaryDepfile() = default;
  TemporaryDepfile(const TemporaryDepfile &) = delete;
  TemporaryDepfile &operator=(const TemporaryDepfile &) = delete;
  TemporaryDepfile(TemporaryDepfile &&) = delete;
  TemporaryDepfile &operator=(TemporaryDepfile &&) = delete;
  ~TemporaryDepfile() {
    if (directory_.empty()) return;
    std::error_code ec;
    std::filesystem::remove(file_, ec);
    std::filesystem::remove(directory_, ec);
  }

  bool Create(const std::string &destination) {
    std::random_device random;
    for (int attempt = 0; attempt < 64; ++attempt) {
      const std::filesystem::path candidate = destination + ".tmp-" +
                                              std::to_string(random()) + "-" +
                                              std::to_string(random());
      std::error_code ec;
      if (std::filesystem::create_directory(candidate, ec)) {
        directory_ = candidate;
        file_ = directory_ / "depfile";
        return true;
      }
      if (ec && ec != std::errc::file_exists) {
        Logger::LogRawError(
            "failed to create dependency temporary directory: " + ec.message());
        return false;
      }
    }
    Logger::LogRawError(
        "failed to create unique dependency temporary directory");
    return false;
  }

  [[nodiscard]] const std::filesystem::path &file() const { return file_; }

 private:
  std::filesystem::path directory_;
  std::filesystem::path file_;
};

}  // namespace

bool WriteDepfile(const std::string &output,
                  const std::vector<std::filesystem::path> &dependencies) {
  const auto file = output + ".d";
  // Never replace a source with a dependency file.
  for (const auto &dependency : dependencies) {
    std::error_code ec;
    if (std::filesystem::equivalent(file, dependency, ec)) {
      Logger::LogRawError("dependency file conflicts with an input file");
      return false;
    }
  }
  TemporaryDepfile temporary;
  if (!temporary.Create(file)) return false;
  std::ofstream out(temporary.file());
  if (!out) {
    Logger::LogRawError("failed to open temporary dependency file");
    return false;
  }
  // Preserve the main source first; sort and deduplicate the remaining files.
  std::set<std::string> imports;
  for (const auto &dependency : dependencies) {
    if (dependency != dependencies.front()) imports.insert(dependency.string());
  }
  out << output << ":";
  if (!dependencies.empty()) out << " " << dependencies.front().string();
  for (const auto &dependency : imports) out << " \\\n  " << dependency;
  out << "\n";
  for (const auto &dependency : imports) out << "\n" << dependency << ":\n";
  out.close();
  if (!out) {
    Logger::LogRawError("failed to write dependency file");
    return false;
  }
  std::error_code ec;
  std::filesystem::rename(temporary.file(), file, ec);
  if (ec) {
    Logger::LogRawError("failed to replace dependency file: " + ec.message());
    return false;
  }
  return true;
}

}  // namespace yulang::front
