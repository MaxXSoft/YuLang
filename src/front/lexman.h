#ifndef YULANG_FRONT_LEXMAN_H_
#define YULANG_FRONT_LEXMAN_H_

#include <vector>
#include <string>
#include <filesystem>
#include <optional>
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

  // add a path as a new import path, returns true if success
  bool AddImportPath(int priority, const std::filesystem::path &path);
  // load source file, returns true if success
  bool LoadSource(const std::filesystem::path &file);
  // get module path by module name, returns empty path if not found
  std::filesystem::path GetModPath(const ModName &mod_name);
  // check if current module has already been loaded
  bool IsLoaded(const std::filesystem::path &file);
  // set or create current lexer by path, returns last lexer
  // return 'nullopt' if file does not exists
  std::optional<LexerPtr> SetLexer(const std::filesystem::path &file);
  // set current lexer, returns last lexer
  LexerPtr SetLexer(const LexerPtr &lexer);

  // getters
  // current lexer
  const LexerPtr &lexer() const { return lexer_; }

 private:
  // import path records
  std::multimap<int, std::filesystem::path, std::greater<int>> imp_paths_;
  // all loaded lexers
  std::unordered_map<std::string, LexerPtr> lexers_;
  // current lexer
  LexerPtr lexer_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_LEXMAN_H_
