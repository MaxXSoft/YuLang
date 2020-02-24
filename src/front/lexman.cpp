#include "front/lexman.h"

using namespace yulang::front;

void LexerManager::AddImportPath(int priority,
                                 const std::filesystem::path &path) {
  for (const auto &i : import_paths_) {
    if (i.second == path) return;
  }
  import_paths_.insert({priority, path});
}

bool LexerManager::LoadDefauleSource(const std::filesystem::path &file) {
  AddImportPath(0, file.parent_path());
  if (!SetLexer(file)) return false;
  default_lexer_ = lexer_;
}

bool LexerManager::SetLexer(const ModName &mod_name) {
  if (mod_name.empty()) {
    // reset current lexer to default
    lexer_ = default_lexer_;
    return true;
  }
  else {
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
    for (const auto &i : import_paths_) {
      auto file = i.second / mod_path;
      if (std::filesystem::exists(file)) return SetLexer(file);
    }
    return false;
  }
}

bool LexerManager::SetLexer(const std::filesystem::path &file) {
  // find specific lexer
  auto file_str = file.string();
  auto it = lexers_.find(file_str);
  if (it == lexers_.end()) {
    // not found, create lexer
    lexer_ = std::make_shared<Lexer>(file_str);
    lexers_.insert({file_str, lexer_});
  }
  else {
    // just set
    lexer_ = it->second;
  }
  return true;
}
