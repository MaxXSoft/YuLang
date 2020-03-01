#include "front/lexman.h"

#include <utility>

using namespace yulang::front;

namespace {

inline std::filesystem::path GetFullPath(const std::filesystem::path &p) {
  return std::filesystem::canonical(std::filesystem::absolute(p));
}

}  // namespace

void LexerManager::AddImportPath(int priority,
                                 const std::filesystem::path &path) {
  auto full_path = GetFullPath(path);
  for (const auto &[_, p] : imp_paths_) {
    if (p == full_path) return;
  }
  imp_paths_.insert({priority, full_path});
}

bool LexerManager::LoadSource(const std::filesystem::path &file) {
  auto full_path = GetFullPath(file);
  AddImportPath(0, full_path.parent_path());
  return !!SetLexer(full_path);
}

std::filesystem::path LexerManager::GetModPath(const ModName &mod_name) {
  // get relative path of specific module
  std::filesystem::path mod_path;
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

bool LexerManager::IsLoaded(const std::filesystem::path &file) {
  return lexers_.find(file.string()) != lexers_.end();
}

LexerPtr LexerManager::SetLexer(const std::filesystem::path &file) {
  auto last = lexer_;
  // find specific lexer
  auto file_str = file.string();
  auto it = lexers_.find(file_str);
  if (it == lexers_.end()) {
    // not found, create lexer
    auto [it, _] = lexers_.insert({file_str, nullptr});
    lexer_ = std::make_shared<Lexer>(it->first);
    it->second = lexer_;
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
