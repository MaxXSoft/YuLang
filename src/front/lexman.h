#ifndef YULANG_FRONT_LEXMAN_H_
#define YULANG_FRONT_LEXMAN_H_

#include <vector>
#include <string>
#include <filesystem>
#include <map>
#include <unordered_map>

#include "front/lexer.h"

namespace yulang::front {

// type of module name (the module representation behind 'import')
using ModName = std::vector<std::string>;

// factory class of 'Lexer'
class LexerManager {
 public:
  LexerManager() { AddImportPath(0, std::filesystem::current_path()); }
  LexerManager(const std::filesystem::path &file) {
    AddImportPath(0, file);
    AddImportPath(0, std::filesystem::current_path());
  }

  // add a path as a new import path
  void AddImportPath(int priority, const std::filesystem::path &path);
  // load default source file
  bool LoadDefauleSource(const std::filesystem::path &file);
  // set or create current lexer by module name
  bool SetLexer(const ModName &mod_name);
  // set or create current lexer by path
  bool SetLexer(const std::filesystem::path &file);

  // getters
  // current lexer
  const LexerPtr &lexer() const { return lexer_; }

 private:
  // import path records
  std::map<int, std::filesystem::path, std::greater<int>> import_paths_;
  // all loaded lexers
  std::unordered_map<std::string, LexerPtr> lexers_;
  // default/current lexer
  LexerPtr default_lexer_, lexer_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_LEXMAN_H_
