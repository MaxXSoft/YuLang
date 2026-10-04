#include "front/lexman.h"

#include <algorithm>
#include <array>
#include <fstream>
#include <utility>

#include "define/token.h"
#include "front/unicode.h"

namespace yulang::front {

namespace {

using Path = std::filesystem::path;

constexpr std::string_view kMarkerSuffix = ".lib.yu";
const std::array kKeywords = {YULANG_KEYWORDS(YULANG_EXPAND_SECOND)};

Path Canonical(const Path &path) {
  std::error_code error;
  const auto result = std::filesystem::canonical(path, error);
  return error ? Path{} : result;
}

bool IsInside(const Path &file, const Path &root) {
  const auto relative = file.lexically_relative(root);
  return !relative.empty() && *relative.begin() != "..";
}

bool IsMarker(const Path &path) {
  const auto name = path.filename().string();
  return name.size() >= kMarkerSuffix.size() &&
         name.compare(name.size() - kMarkerSuffix.size(), kMarkerSuffix.size(),
                      kMarkerSuffix) == 0;
}

bool IsName(std::string_view name) {
  return unicode::IsIdentifier(name) &&
         std::find(kKeywords.begin(), kKeywords.end(), name) == kKeywords.end();
}

bool Fail(std::string_view message) {
  Logger::LogRawError(message);
  return false;
}

Path FindMarker(const Path &root) {
  Path marker;
  std::error_code error;
  std::filesystem::directory_iterator entries(root, error);
  const std::filesystem::directory_iterator end;
  while (!error && entries != end) {
    if (IsMarker(entries->path())) {
      if (!marker.empty()) {
        Fail("library root must contain exactly one .lib.yu marker: " +
             root.string());
        return {};
      }
      marker = entries->path();
    }
    entries.increment(error);
  }
  if (error || marker.empty() ||
      !std::filesystem::is_regular_file(marker, error)) {
    Fail("library root must contain exactly one .lib.yu marker: " +
         root.string());
    return {};
  }
  return marker;
}

bool IsEmptyMarker(const Path &marker) {
  std::ifstream input(marker, std::ios::binary);
  if (!input) return Fail("cannot read library marker: " + marker.string());
  char character{};
  while (input.get(character)) {
    if (character != ' ' && character != '\t' && character != '\n' &&
        character != '\r' && character != '\f' && character != '\v') {
      return Fail("library marker must contain only whitespace: " +
                  marker.string());
    }
  }
  if (!input.eof())
    return Fail("cannot read library marker: " + marker.string());
  return true;
}

}  // namespace

bool LexerManager::AddDefine(std::string_view definition) {
  const auto equal = definition.find('=');
  if (equal == std::string_view::npos || equal == 0) return false;
  const auto name = definition.substr(0, equal);
  if (!unicode::IsIdentifier(name)) return false;
  defines_[std::string(name)] = std::string(definition.substr(equal + 1));
  return true;
}

bool LexerManager::AddImportPath(int /*priority*/, const Path &path) {
  const auto root = Canonical(path);
  std::error_code error;
  if (root.empty() || !std::filesystem::is_directory(root, error)) {
    return Fail("invalid library root: " + path.string());
  }
  for (const auto &library : libraries_) {
    if (library.path == root) return true;
  }

  const auto marker = FindMarker(root);
  if (marker.empty()) return false;
  const auto filename = marker.filename().string();
  const auto name = filename.substr(0, filename.size() - kMarkerSuffix.size());
  if (!IsName(name)) return Fail("invalid library name: " + name);
  if (!IsEmptyMarker(marker)) return false;

  for (const auto &library : libraries_) {
    if (library.name == name) return Fail("duplicate library name: " + name);
    if (IsInside(root, library.path) || IsInside(library.path, root)) {
      return Fail("registered library roots must not overlap: " +
                  root.string());
    }
  }
  libraries_.push_back({name, root, marker});
  if (!dependencies_.empty()) {
    dependencies_.push_back(root);
    dependencies_.push_back(marker);
  }
  return true;
}

bool LexerManager::LoadSource(const Path &file) {
  const auto full_path = Canonical(file);
  std::error_code error;
  if (full_path.empty() ||
      !std::filesystem::is_regular_file(full_path, error)) {
    return false;
  }
  if (IsMarker(file) || IsMarker(full_path)) {
    return Fail("a library marker is not a source module");
  }
  if (!SetLexer(full_path)) return false;
  for (const auto &library : libraries_) {
    for (const auto &dependency : {library.path, library.marker}) {
      if (std::find(dependencies_.begin(), dependencies_.end(), dependency) ==
          dependencies_.end()) {
        dependencies_.push_back(dependency);
      }
    }
  }
  return true;
}

Path LexerManager::GetModPath(const ModName &mod_name) {
  if (mod_name.size() < 2) return {};
  const auto library = std::find_if(
      libraries_.begin(), libraries_.end(),
      [&](const auto &item) { return item.name == mod_name.front(); });
  if (library == libraries_.end()) return {};
  Path relative;
  for (std::size_t i = 1; i < mod_name.size(); ++i) {
    if (!IsName(mod_name[i])) return {};
    relative /= mod_name[i];
    if (i + 1 < mod_name.size()) {
      // Adding a module can turn a previously unique definition import into
      // an ambiguous import. Track existing directories even for absent files.
      const auto directory = Canonical(library->path / relative);
      std::error_code error;
      if (!directory.empty() && IsInside(directory, library->path) &&
          std::filesystem::is_directory(directory, error) &&
          std::find(dependencies_.begin(), dependencies_.end(), directory) ==
              dependencies_.end()) {
        dependencies_.push_back(directory);
      }
    }
  }
  relative += ".yu";
  auto file = Canonical(library->path / relative);
  std::error_code error;
  if (file.empty() || !std::filesystem::is_regular_file(file, error)) return {};
  if (!IsInside(file, library->path)) {
    Fail("import resolves outside its registered library root: " +
         file.string());
    return {};
  }
  return file;
}

std::string LexerManager::ModuleId(const Path &file) const {
  const auto full_path = Canonical(file);
  if (full_path.empty()) return {};
  for (const auto &library : libraries_) {
    if (!IsInside(full_path, library.path)) continue;
    auto relative = full_path.lexically_relative(library.path);
    if (relative.extension() != ".yu" || IsMarker(relative)) return {};
    relative.replace_extension();
    std::string id = library.name;
    for (const auto &part : relative) {
      const auto name = part.string();
      if (!IsName(name)) {
        Fail("invalid module path component: " + name);
        return {};
      }
      id += '.';
      id += name;
    }
    return id;
  }
  return {};
}

bool LexerManager::IsLoaded(const Path &file) {
  return lexers_.find(Canonical(file).string()) != lexers_.end();
}

std::optional<LexerPtr> LexerManager::SetLexer(const Path &file) {
  const auto full_path = Canonical(file);
  if (full_path.empty()) return {};
  auto last = lexer_;
  const auto file_str = full_path.string();
  const auto found = lexers_.find(file_str);
  if (found == lexers_.end()) {
    auto [entry, inserted] = lexers_.insert({file_str, nullptr});
    (void)inserted;
    lexer_ = std::make_shared<Lexer>(entry->first, defines_);
    entry->second = lexer_;
    dependencies_.push_back(full_path);
  } else {
    lexer_ = found->second;
  }
  return last;
}

LexerPtr LexerManager::SetLexer(const LexerPtr &lexer) {
  auto last = lexer_;
  lexer_ = lexer;
  return last;
}

}  // namespace yulang::front
