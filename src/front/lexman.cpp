#include "front/lexman.h"

#include <cctype>
#include <utility>

namespace yulang::front {

namespace {

using Path = std::filesystem::path;

inline Path GetFullPath(const Path &p) {
  return std::filesystem::canonical(std::filesystem::absolute(p));
}

}  // namespace

bool LexerManager::AddDefine(std::string_view definition) {
  const auto equal = definition.find('=');
  if (equal == std::string_view::npos || equal == 0) return false;
  const auto name = definition.substr(0, equal);
  // Check if the name is a valid identifier.
  for (std::size_t i = 0; i < name.size(); ++i) {
    const auto c = static_cast<unsigned char>(name[i]);
    if (c != '_' && !(i ? std::isalnum(c) : std::isalpha(c))) return false;
  }
  defines_[std::string(name)] = std::string(definition.substr(equal + 1));
  return true;
}

bool LexerManager::AddImportPath(int priority, const Path &path) {
  // return false if is invalid path
  if (!std::filesystem::exists(path)) return false;
  // get full path
  const auto full_path = GetFullPath(path);
  // insert if not exits
  for (const auto &[_, p] : imp_paths_) {
    if (p == full_path) return true;
  }
  imp_paths_.insert({priority, full_path});
  return true;
}

bool LexerManager::LoadSource(const Path &file) {
  // return false if is invalid path
  if (!std::filesystem::exists(file)) return false;
  // add to import path
  const auto full_path = GetFullPath(file);
  AddImportPath(0, full_path.parent_path());
  // create new lexer
  return !!SetLexer(full_path);
}

Path LexerManager::GetModPath(const ModName &mod_name) {
  // get relative path of specific module
  Path mod_path;
  for (std::size_t i = 0; i < mod_name.size(); ++i) {
    if (i == mod_name.size() - 1) {
      mod_path /= mod_name[i] + ".yu";
    } else {
      mod_path /= mod_name[i];
    }
  }
  // try to find a valid module on the disk
  for (const auto &[_, path] : imp_paths_) {
    auto file = path / mod_path;
    if (std::filesystem::exists(file)) return file;
  }
  return {};
}

bool LexerManager::IsLoaded(const Path &file) {
  return lexers_.find(file.string()) != lexers_.end();
}

std::optional<LexerPtr> LexerManager::SetLexer(const Path &file) {
  auto last = lexer_;
  // find specific lexer
  const auto file_str = file.string();
  const auto it = lexers_.find(file_str);
  if (it == lexers_.end()) {
    if (!std::filesystem::exists(file)) return {};
    // not found, create lexer
    auto [it, _] = lexers_.insert({file_str, nullptr});
    lexer_ = std::make_shared<Lexer>(it->first, defines_);
    it->second = lexer_;
    dependencies_.push_back(std::filesystem::absolute(file).lexically_normal());
  } else {
    // just set
    lexer_ = it->second;
  }
  return last;
}

// set current lexer
LexerPtr LexerManager::SetLexer(const LexerPtr &lexer) {
  auto last = lexer_;
  lexer_ = lexer;
  return last;
}

}  // namespace yulang::front
