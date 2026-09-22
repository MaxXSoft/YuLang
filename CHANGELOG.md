# Changelog

All notable changes to the YuLang compiler will be documented in this file.

## Unreleased

### Added

* C/C++ static analysis checks with clang-tidy 23.
* Supported `-MD` command line option for generating Makefile dependencies.
* Supported `-D` command line option for definiting simple macros.
* Native runtime regression suite (`ctest -L runtime`).

### Changed

* Updated submodule XSTL.
* Matched `lib/sys` `long`, `unsigned long` and `time_t` bindings (e.g. `time`, `strtol`, `strtoul`, `lround`) to pointer-width types for the 64-bit Unix ABI.
* Batched `IO` string output using `strlen` and completed short/partial writes instead of writing one byte at a time.
* Extended clang-tidy static analysis to the runtime test helpers.

### Fixed

* Constant numeric evaluation now preserves operand types and widths, truncates results to the target type, and bails out on `INT_MIN / -1`, division or modulo by zero, and out-of-range shifts.
* Integer constants are truncated to their LLVM type width.
* Preserved `f32` precision when re-evaluating folded literals.
* Constant evaluation now respects lexical shadowing, including `for-in` loop variables.
* `when` expression folding preserves branch selection and side effects, keeping unknown conditions that precede a match.
* Volatile reads are preserved during evaluation without adding extra lvalue reads.
* Structure layouts now respect target ABI alignment.
* Global initialization continues from the current control-flow tail.
* Runtime boolean conversions to `bool` and from `bool` to other numeric types.
* `DynArray` growth (no longer under-sizes on large requests), copying and self-assignment.
* `Stack` correctly resets the frame cursor at frame boundaries and preserves stack frames.
* `HashMap` grows by element load instead of occupied bucket count, and rejects duplicate keys whose stored value is null.

## 0.0.8 - 2026-09-12

### Added

* CTest coverage for all examples and LLVM backend regression tests at O0-O3, including object-file comparisons with `llc`.
* CI builds for LLVM 21, 22 and 23, and C/C++ formatting checks with clang-format 23.

### Changed

* Raised the minimum required versions to LLVM 21 and CMake 3.28.
* Migrated the LLVM backend to current APIs, opaque pointers and the new IR pass manager's default optimization pipelines.
* Applied `-O 0` through `-O 3` to both IR optimization and machine-code generation, using the same backend pipeline as `llc`.
* Unified compiler, standard library and example builds in CMake, replacing the handwritten Makefile and toolchain configuration. Standard library and example builds now emit object files directly with `yuc`.
* Moved test runners and fixtures into `tests/backend` and `tests/example`, replacing the shell test runner. Backend test artifacts now use the CMake build directory.
* Updated C/C++ formatting and preserved the readable layout of keyword and operator macros.

### Fixed

* Invalid scalar and null-pointer zero constants that caused assembly and object-file emission to fail.
* Missing or truncated text output caused by exiting before output streams were flushed.

## 0.0.7 - 2023-07-12

### Changed

* Supported up to LLVM 16.

## 0.0.6 - 2022-08-28

### Changed

* Supported LLVM 12, 13, 14 and 15.

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
