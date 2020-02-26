#include "front/lexman.h"

using namespace yulang::front;

void LexerManager::AddImportPath(int priority,
                                 const std::filesystem::path &path) {
  for (const auto &[_, p] : import_paths_) {
    if (p == path) return;
  }
  import_paths_.insert({priority, path});
}

bool LexerManager::LoadSource(const std::filesystem::path &file) {
  AddImportPath(0, file.parent_path());
  return !!SetLexer(file);
}

LexerPtr LexerManager::SetLexer(const ModName &mod_name) {
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
  for (const auto &[_, path] : import_paths_) {
    auto file = path / mod_path;
    if (std::filesystem::exists(file)) return SetLexer(file);
  }
  return nullptr;
}

LexerPtr LexerManager::SetLexer(const std::filesystem::path &file) {
  auto last = lexer_;
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
  return last;
}

// set current lexer
LexerPtr LexerManager::SetLexer(const LexerPtr &lexer) {
  auto last = lexer_;
  lexer_ = lexer;
  return last;
}
