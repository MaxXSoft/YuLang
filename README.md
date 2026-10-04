# Yu

![YuLang](YuLang.png)

[![Build and Test](https://github.com/MaxXSoft/YuLang/workflows/Build%20and%20Test/badge.svg)](https://github.com/MaxXSoft/YuLang)
[![羽语言简明教程](https://img.shields.io/badge/tutorial-%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87-blue)](https://maxxsoft.github.io/YuLang-doc/tutorial/zh-cn/)

Yu (羽) is a simple system programming language.

## Documentations

Tutorial: [羽语言简明教程 (简体中文)](https://maxxsoft.github.io/YuLang-doc/tutorial/zh-cn/).

Visit [YuLang-doc](https://github.com/MaxXSoft/YuLang-doc) for more details.

## Features

* Block expression
* Modules and `import` statement
* Type deduction and reference type
* Inline function/variable/constant
* Function/operator overloading
* Operator-formed identifier
* Dot function and infix function
* Iterator
* Contextual integer and null literals
* Unicode XID identifiers

Ordinary identifiers use Unicode 17.0.0 `XID_Start` (or `_`) followed by zero or
more `XID_Continue` characters, encoded as valid UTF-8. This also applies to
module names and `-D` macro names. Identifier spelling is preserved without
normalization or case folding: `é` and `e` followed by a combining acute accent
are distinct names. Operator-formed identifiers retain their existing syntax.
Diagnostic columns count source bytes.

The Unicode tables are checked in, so builds do not depend on the host's Unicode
version or require downloading data. To regenerate them, download the versioned
`DerivedCoreProperties.txt` linked in `tools/generate_unicode_xid.py` and pass
its path to that script; it verifies the source checksum.

Inside function bodies, `let` and `var` may shadow either kind of earlier
binding, including a parameter. The initializer sees the previous binding;
later statements see the new binding, which may have a different type. Each
binding has its own storage, so existing pointers and references retain their
original referent. Leaving a nested block restores the enclosing binding.
Mutability is determined by the new declaration. Global duplicate definitions
and duplicate parameters remain errors.

```yu
def positive(value: i32,): bool {
  var value = value + 1
  value += 1
  let value = value > 0
  value
}
let result = positive(2,)
extern declare callback: (i32,): bool
```

Function parameters, call arguments (including dot calls), and function-type
parameter lists allow a trailing comma after the last element. Empty lists
still use `()`; `(,)` and repeated commas are invalid.

The frontend reports multiple independent errors in one invocation. Parsing
resumes at statement or enclosing block boundaries, including inside functions
and imported modules. Semantic analysis continues with independent statements
and definitions. A malformed function is discarded after checking its syntax;
dependent errors within one expression or an invalid declaration are not all
reported. Any frontend error makes compilation fail without emitting AST, IR,
assembly, object code, or dependency files.

Integer literals take their type from a declaration, assignment, return value,
parameter, aggregate element, or the other operand of an operation. The context
also reaches arithmetic expressions and the result of blocks, `if`, and `when`:

```yu
var count: usize = 1 + 2
count += 1
let mask: u64 = 1 << 40
var ptr: u8* = null
let empty = ptr == null
```

Without a context, integers still default to `i32`; `let ptr = null` requires a
type annotation. Contextually typed literals must fit the target integer type,
including the target's `usize`/`isize` width. Ordinary variables, named constants,
explicit `as` conversions, characters, and floating-point values retain their
types. Explicit casts retain their existing truncation behavior.

Function and operator calls prefer an existing exact parameter match. Otherwise,
only literal-compatible candidates are considered; exactly one must match.
Ambiguous calls require an explicit type. Literal typing does not create temporary
references or remove pointer target qualifiers.

Structure and array initializers may omit the type inside brackets when the
context supplies it:

```yu
struct Point { x: i32, y: i32 }
def origin(): Point { [] {} }
var point: Point = [] {1, 2}
point = [] {3}  // omitted members are still zero-initialized
var points: Point[2] = [] {[] {4, 5}, [] {6, 7}}
```

Declarations, assignments, returns, nested aggregate members, and function
parameters provide this context, including through block, `if`, and `when`
results. For overloaded calls, other arguments must narrow the candidates to
one type for each omitted initializer. Initializer contents do not select a
structure by its shape. Use `[Type] {...}` when the type is ambiguous or absent,
such as `let point = [Point] {1, 2}`. This syntax creates values and cannot bind
temporary references or initialize scalar types.

Libraries are registered with `-I DIR`. Each root contains exactly one empty
`<name>.lib.yu` marker (only whitespace is allowed in its contents). The standard
library is registered with `-I lib` and its marker is `lib/std.lib.yu`.
Imports always start with the library name, including imports within a library:

```yu
import std.dynarray as da
import std.range.until
import std.io.{out, err as errors}
import std.sys.ctype.*

var values = da.newDynArray()
for i in 0 until 10 { values.push(i) }
```

A module import binds a namespace; it does not inject its definitions. Selected
and wildcard imports bind public definitions in the current module only.
Imports cannot be public or re-export another module's bindings. An import that
could name both a module and a definition is an error, including with `as`;
rename one of the conflicting targets. Registering two different roots with the
same library name is also an error. Marker files are not compiled as modules.
An import must reach a `.yu` file or one of its public definitions: a library
name or a directory alone cannot be imported. Import cycles and overlapping
registered roots are rejected.

Compile library files with their own root registered too, so direct compilation
and importing produce the same module identity. An input outside registered
roots is an anonymous compilation unit and needs no marker; it can still import
registered libraries. Its own non-`extern` definitions have internal linkage.
There is no separate library-versus-executable source classification.

Definitions and structure fields are private to their defining module unless
marked `public`. Visibility is independent of `extern` and `inline`: exported C
declarations use `public extern declare`, while a private C entry point may use
`extern def main`. Inline functions intended for callers use `public inline def`.
Public types may contain private fields and internal helper types; public fields,
function signatures, globals and aliases cannot expose private nominal types.
Construct a structure with private fields through a function in its defining
module rather than an aggregate initializer in another module. This also checks
omitted array elements and omitted public aggregate fields. A `var` declaration
without an initializer retains the existing zero-valued storage behavior;
field visibility does not introduce constructor validation.

Structures and enumerations have declaration identity: equal layouts in different
modules are different types. Transparent type aliases and renamed imports keep
the original identity. Library names and module-relative paths are part of the
ABI, so changing either requires rebuilding callers; moving a library root does
not change its identity.

`value.method(args)` searches visible functions and public functions in the
receiver type's defining module. Named infix calls and the `next`/`last` iterator
protocol use the same lookup; ambiguous overloads require an explicit module
qualification. Functions on primitive values, such as `until`, still need a
selected or wildcard import. `module.function(args)` is a qualified call without
an inserted receiver, and `(value.callback)(args)` calls a function-valued field.
Bare `function(value, args)` uses names visible in the current scope. Symbolic
operators consider both operand owners, and receiver lookup follows pointer and
reference wrappers without inserting additional dereferences or conversions.

The compiler scans complete modules, including private declarations and imports.
Module-level definitions can refer to later definitions; overload resolution
uses the complete overload set. Local bindings still become visible after their
initializers. Required constant and by-value layout cycles are errors; pointer
recursion is allowed. Dynamic global initialization retains source order.

Imported inline bodies keep the defining module's bindings and access rights.
Private helpers and globals referenced by copied bodies receive canonical
linker-visible symbols while remaining private in YuLang. Global storage and
initialization are emitted once, in the owner. Link the owner object when inline
code refers to its ordinary helpers or stored globals. Purely folded constants
and layout calculations do not require such support symbols.

<details>
  <summary>EBNF of Yu</summary>

  ```ebnf
  program   ::= {line};
  line      ::= stmt {";" stmt} [";"];
  stmt      ::= var_def   | let_def | fun_def | declare
              | ty_alias  | struct  | enum    | import;

  var_def   ::= property "var" var_elem {"," var_elem};
  let_def   ::= property "let" let_elem {"," let_elem};
  fun_def   ::= property "def" [id | bin_op | unary_op]
                "(" [arg_list [","]] ")" [":" type] block;
  declare   ::= property "declare" ["var"] id ":" type;
  ty_alias  ::= property "type" id "=" type;
  struct    ::= ["public"] "struct" id "{" [field_list [","]] "}";
  enum      ::= property "enum" id [":" type] "{" enum_list "}";
  import    ::= "import" path ["as" id | ".*" | ".{" import_list [","] "}"];

  property  ::= ["public"] ["extern" | "inline"];
  path      ::= id {"." id};
  import_list ::= id ["as" id] {"," id ["as" id]};
  field_list ::= ["public"] id ":" type {"," ["public"] id ":" type};
  var_elem  ::= id [":" type] ["=" expr];
  let_elem  ::= id [":" type] "=" expr;
  arg_list  ::= id ":" type ["," arg_list];
  enum_list ::= id ["=" expr] ["," enum_list] [","];

  block     ::= "{" {blk_line} "}";
  blk_line  ::= blk_stmt {";" blk_stmt} [";"];
  blk_stmt  ::= var_def | let_def | declare | ty_alias  | struct
              | enum    | if_else | when    | while     | for_in
              | asm     | control | expr;

  if_else   ::= "if" expr block ["else" (if_else | block)];
  when      ::= "when" expr "{" when_elem {when_elem} ["else" block] "}";
  while     ::= "while" expr block;
  for_in    ::= "for" id "in" expr block;
  asm       ::= "asm" "{" string {string} "}";
  control   ::= "break" | "continue"  | ("return" [expr]);

  when_elem ::= expr {"," expr} block;

  expr      ::= binary {id binary};
  binary    ::= cast {bin_op cast};
  cast      ::= unary {"as" type};
  unary     ::= [unary_op] factor | "sizeof" type;
  factor    ::= value | block     | if_else   | when
              | index | fun_call  | access    | "(" expr ")";

  bin_op    ::= "+"   | "-"   | "*"   | "/"   | "%"   | "&"
              | "|"   | "^"   | "&&"  | "||"  | "<<"  | ">>"
              | "=="  | "!="  | "<"   | "<="  | ">"   | ">="
              | "="   | "+="  | "-="  | "*="  | "/="  | "%="
              | "&="  | "|="  | "^="  | "<<=" | ">>=" | ".";
  unary_op  ::= "+"   | "-"   | "!"   | "~"   | "*"   | "&";
  index     ::= factor "[" expr "]";
  fun_call  ::= factor "(" [expr {"," expr} [","]] ")";
  access    ::= factor "." id ["(" [expr {"," expr} [","]] ")"];

  value     ::= INT_VAL | FLOAT_VAL | CHAR_VAL | id
              | string  | bool      | null_ptr | val_init;
  id        ::= ID_VAL;
  string    ::= STR_VAL;
  bool      ::= "true"  | "false";
  null_ptr  ::= "null";
  val_init  ::= "[" [type] "]" "{" [expr {"," expr} [","]] "}";

  type      ::= (prim_type | path | pointer | array | ref | func) ["volatile"];
  prim_type ::= "i8"  | "i16" | "i32"   | "i64" | "isize" | "u8"  | "u16"
              | "u32" | "u64" | "usize" | "f32" | "f64" | "bool";
  pointer   ::= type ["var"] "*";
  array     ::= type "[" expr "]";
  ref       ::= type ["var"] "&";
  func      ::= "(" [type {"," type} [","]] ")" [":" type];
  ```
</details>

## Examples: Hello World

C style:

```yu
import std.sys.unistd.{FD_STDOUT, write}
import std.sys.string.strlen

extern def main(argc: i32, argv: u8**): i32 {
  let hello = "Hello world!\n", len = strlen(hello)
  write(FD_STDOUT, hello, len)
  0
}
```

OOP-like style (but not real OOP):

```yu
import std.sys.unistd.{FD_STDOUT, write}
import std.sys.string.strlen

struct Output {}

def println(this: Output&, str: u8*) {
  write(FD_STDOUT, str, strlen(str))
  write(FD_STDOUT, "\n", 1)
}

let out = [Output] {}

extern def main(argc: i32, argv: u8**): i32 {
  out.println("Hello world!")
  0
}
```

C++ `cout` style with custom operator `<<<`:

```yu
import std.io.out

extern def main(argc: i32, argv: u8**): i32 {
  out <<< "Hello world! " <<< 123 <<< '\n'
  0
}
```

Natural language style (see [natural.yu](examples/natural.yu)):

```yu
extern def main(argc: i32, argv: u8**): i32 {
  // be polite
  please put "Hello world! " and 123 to stdout
  thanks
}
```

## Build from Source

Before building YuLang compiler, please make sure you have installed the following dependencies:

* `cmake` 3.28 or later
* `llvm` 21 or later
* C and C++ compilers, with C++17 support
* Python 3 for tests (or configure with `-DBUILD_TESTING=OFF`)

CMake builds the compiler, standard library and examples together:

```sh
git clone --recursive https://github.com/MaxXSoft/YuLang.git
cd YuLang
cmake -S . -B build
cmake --build build -j
ctest --test-dir build --output-on-failure -j
```

The standard library and examples use Yu optimization level O0 for Debug and O2 for other configurations. Select a configuration with `-DCMAKE_BUILD_TYPE=Debug` or `-DCMAKE_BUILD_TYPE=Release`.

You can build just the standard library with `cmake --build build --target yu`, or all examples with `--target yu_examples`. Each example also has an `example_<name>` target. For multi-configuration generators, use `cmake --build build --config Debug` and `ctest --test-dir build -C Debug`; executables are placed in the corresponding configuration subdirectory.

With Homebrew LLVM (including LLVM 23), select its CMake package explicitly:

```sh
cmake -S . -B build -DLLVM_DIR="$(brew --prefix llvm)/lib/cmake/llvm"
cmake --build build -j
ctest --test-dir build --output-on-failure -j
```

## Test and Check

The `lib/sys` C bindings require a Unix ABI where `long`, `unsigned long`, and `time_t` have pointer width, as on 64-bit Linux and macOS. Targets with LLP64 or 32-bit time64 ABIs need matching target-specific declarations; `-tt` selects the machine target, but does not infer libc typedefs or time64 symbol redirection. The `runtime.libc_abi` test checks the bindings against the native C headers and runtime.

Use `ctest --test-dir build -L examples` to run only examples, `-L backend` for backend tests, or `-R example.io_test` to select one test.

Use `-L runtime` for the compiler and standard-library regressions. These compare native Yu results with C checks at O0 and O2, check volatile access counts, and compare structure sizes with LLVM layouts on available targets.

For example, to compile and link a Yu program:

```sh
build/yuc -I lib -O 2 -ot obj examples/reduce.yu -o build/reduce.o
clang build/reduce.o -Lbuild -lyu -o build/reduce
```

Use `-D NAME=TEXT` (or `--define NAME=TEXT`) to replace ordinary identifiers in the main source and imported files:

```sh
build/yuc main.yu -D SIZE=16 -D VALUE='1 + 2' -o main.o
```

Definitions require `=`; an empty replacement is allowed, and the last definition of a name wins. Names must use identifier syntax. Keywords, strings, and comments are not replaced. Replacement text is lexed normally but is not expanded again, so `-D A=B -D B=1` replaces `A` with `B`. Replacement tokens stay separate from surrounding source tokens; no parentheses are added. Use a separate argument after `-D`, not `-DNAME=TEXT`.

Use clang-format for C/C++ formatting. CI checks all tracked project source and header files against `.clang-format`; submodules and build artifacts are excluded. To run the same check locally (use `clang-format-23` if your system installs the tool under a versioned name):

```sh
git ls-files -z -- '*.c' '*.cc' '*.cpp' '*.cxx' '*.h' '*.hh' '*.hpp' '*.hxx' '*.inc' |
  xargs -0 clang-format --style=file --dry-run --Werror --fail-on-incomplete-format
```

For static analysis, use clang-tidy 23 and a CMake compilation database:

```sh
cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
clang-tidy --verify-config
run-clang-tidy -p build -j 4
clang-tidy tests/backend/check_zeros.c tests/runtime/*.c -- -std=c11
```

The last command checks the C helpers that the Python tests compile separately. With Homebrew LLVM on macOS, put its `bin` directory on `PATH` and pass the SDK explicitly when the compilation database does not specify one:

```sh
export PATH="$(brew --prefix llvm)/bin:$PATH"
run-clang-tidy -p build -j 4 \
  -extra-arg=-isysroot -extra-arg="$(xcrun --show-sdk-path)"
clang-tidy tests/backend/check_zeros.c tests/runtime/*.c -- -std=c11 \
  -isysroot "$(xcrun --show-sdk-path)"
```

All enabled tidy warnings are errors. Keep suppressions local, name the exact check in `NOLINT`, and explain the invariant or ownership convention. CTest also covers use-def relocation/allocation failure (`mid.usedef`) and invalid constant expressions (`front.constants`).

## Changelog

See [CHANGELOG.md](CHANGELOG.md)

## License

Copyright (C) 2020-2026 MaxXing. License GPLv3.
