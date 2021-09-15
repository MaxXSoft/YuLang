# Changelog

All notable changes to the YuLang compiler will be documented in this file.

## 0.0.5 - 2021-09-15

### Added

* More declarations of C standard library functions.
* Read operations of `IO` library.
* Pointer-sized type `isize` and `usize`.

### Changed

* A warning will be given when encountering a combination of `extern`/`inline` and `import`.
* Some method name in standard library module `strview`.

### Fixed

* Value evaluation process on `VarLetDefAST`.
* Bugs about counting down in standard library module `range`.
* The process of handling reference types on `VarLetElemAST`.
* Value evaluation process on `IntAST`.
* Bugs about generating code on global constructor and global constant string.
* Bugs about casting integers to booleans.

## 0.0.4 - 2021-03-05

### Added

* Relational operations between pointers.
* Perform multiple type casting operations at once (e.g. `x as u8 as i32`).
* Modules in standard library (`Queue`, `StrView`, `HashMap`)

### Changed

* Allowed type casting from functions/arrays to pointers.
* Allowed type casting from basic types to enumerations.

### Fixed

* Bugs about IR generation on `when` statements, global variables/constants, enumerations and function calls.
* Bugs about loads/stores of `volatile` type.
* Comparison between `struct` types (`IsIdentical`).
* Some details when the parser encounters a newline character.
* Bugs about type casting.
* Bugs about CFG simplification (`BlockMerge` pass).
* Value evaluation process on `ImportAST`.
* Bugs about file existence check.
* Bugs about alignment of structures.
* Compilation errors in some LLVM versions.

## 0.0.3 - 2020-04-03

### Added

* Yu IR: intermediate representation of YuLang.
* New mid-end, which can convert AST to Yu IR.
* Pass manager and passes.
* Back-end, which can convert Yu IR to LLVM IR, assembly or object file.

### Changed

* Removed old AST to LLVM IR back-end.

## 0.0.2 - 2020-03-23

## 0.0.1 - 2020-03-01
