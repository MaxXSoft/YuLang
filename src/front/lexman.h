#ifndef YULANG_FRONT_LEXMAN_H_
#define YULANG_FRONT_LEXMAN_H_

#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "front/lexer.h"

namespace yulang::front {

// type of module name (the module representation behind 'import')
using ModName = std::vector<std::string>;

// factory class of 'Lexer'
class LexerManager {
 public:
  struct LibraryRoot {
    std::string name;
    std::filesystem::path path;
    std::filesystem::path marker;
  };

  LexerManager() = default;

  // add a path as a new import path, returns true if success
  bool AddImportPath(int priority, const std::filesystem::path &path);
  // Add NAME=TEXT before loading source files; the last definition wins.
  bool AddDefine(std::string_view definition);
  // load source file, returns true if success
  bool LoadSource(const std::filesystem::path &file);
  // get module path by module name, returns empty path if not found
  std::filesystem::path GetModPath(const ModName &mod_name);
  // Stable library-qualified module identity, or empty for an anonymous input.
  [[nodiscard]] std::string ModuleId(const std::filesystem::path &file) const;
  [[nodiscard]] const std::vector<LibraryRoot> &libraries() const {
    return libraries_;
  }
  // check if current module has already been loaded
  bool IsLoaded(const std::filesystem::path &file);
  // set or create current lexer by path, returns last lexer
  // return 'nullopt' if file does not exists
  std::optional<LexerPtr> SetLexer(const std::filesystem::path &file);
  // set current lexer, returns last lexer
  LexerPtr SetLexer(const LexerPtr &lexer);

  // getters
  // current lexer
  [[nodiscard]] const LexerPtr &lexer() const { return lexer_; }
  // Source first, followed by imported files in load order.
  [[nodiscard]] const std::vector<std::filesystem::path> &dependencies() const {
    return dependencies_;
  }

 private:
  MacroDefinitions defines_;
  // Registered roots are identities, not an ordered search path.
  std::vector<LibraryRoot> libraries_;
  // all loaded lexers
  std::unordered_map<std::string, LexerPtr> lexers_;
  std::vector<std::filesystem::path> dependencies_;
  // current lexer
  LexerPtr lexer_;
};

}  // namespace yulang::front

#endif  // YULANG_FRONT_LEXMAN_H_
