#include "front/lexman.h"

#include <utility>

using namespace yulang::front;

namespace {

using Path = std::filesystem::path;

inline Path GetFullPath(const Path &p) {
  return std::filesystem::canonical(std::filesystem::absolute(p));
}

}  // namespace

bool LexerManager::AddImportPath(int priority, const Path &path) {
  // return false if is invalid path
  if (!std::filesystem::exists(path)) return false;
  // get full path
  auto full_path = GetFullPath(path);
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
  auto full_path = GetFullPath(file);
  AddImportPath(0, full_path.parent_path());
  // create new lexer
  return !!SetLexer(full_path);
}

Path LexerManager::GetModPath(const ModName &mod_name) {
  // get relative path of specific module
  Path mod_path;
  for (int i = 0; i < mod_name.size(); ++i) {
    if (i == mod_name.size() - 1) {
      mod_path /= mod_name[i] + ".yu";
    }
    else {
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
  auto file_str = file.string();
  auto it = lexers_.find(file_str);
  if (it == lexers_.end()) {
    // not found, create lexer
    auto [it, _] = lexers_.insert({file_str, nullptr});
    lexer_ = std::make_shared<Lexer>(it->first);
    it->second = lexer_;
    // check if path is valid
    if (!std::filesystem::exists(file)) return {};
  }
  else {
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
