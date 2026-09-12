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

## Examples: Hello World

C style:

```yu
import sys.unistd
import sys.string

extern def main(argc: i32, argv: u8**): i32 {
  let hello = "Hello world!\n", len = strlen(hello)
  write(FD_STDOUT, hello, len)
  0
}
```

OOP-like style (but not real OOP):

```yu
import sys.unistd
import sys.string

struct Output {}

def println(this: Output&, str: u8*) {
  write(FD_STDOUT, str, strlen(str))
  write(FD_STDOUT, "\n", 1 as u32)
}

let out = [Output] {}

extern def main(argc: i32, argv: u8**): i32 {
  out.println("Hello world!")
  0
}
```

C++ `cout` style with custom operator `<<<`:

```yu
import io

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

## Building from Source

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

Use `ctest --test-dir build -L examples` to run only examples, `-L backend` for backend tests, or `-R example.io_test` to select one test.

For example, to compile and link a Yu program:

```sh
build/yuc -I lib -O 2 -ot obj examples/reduce.yu -o build/reduce.o
clang build/reduce.o -Lbuild -lyu -o build/reduce
```

Use clang-format for C/C++ formatting. CI checks all tracked project source and header files against `.clang-format`; submodules and build artifacts are excluded. To run the same check locally (use `clang-format-23` if your system installs the tool under a versioned name):

```sh
git ls-files -z -- '*.c' '*.cc' '*.cpp' '*.cxx' '*.h' '*.hh' '*.hpp' '*.hxx' '*.inc' |
  xargs -0 clang-format --style=file --dry-run --Werror --fail-on-incomplete-format
```

## EBNF of Yu

```ebnf
program   ::= {line};
line      ::= stmt {";" stmt} [";"];
stmt      ::= var_def   | let_def | fun_def | declare
            | ty_alias  | struct  | enum    | import;

var_def   ::= property "var" var_elem {"," var_elem};
let_def   ::= property "let" let_elem {"," let_elem};
fun_def   ::= property "def" [id | bin_op | unary_op]
              "(" [arg_list] ")" [":" type] block;
declare   ::= property "declare" ["var"] id ":" type;
ty_alias  ::= property "type" id "=" type;
struct    ::= property "struct" id "{" arg_list [","] "}";
enum      ::= property "enum" id [":" type] "{" enum_list "}";
import    ::= property "import" id {"." id};

property  ::= ["public" | "extern" | "inline"]
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
fun_call  ::= factor "(" [expr {"," expr}] ")";
access    ::= factor "." id ["(" [expr {"," expr}] ")"];

value     ::= INT_VAL | FLOAT_VAL | CHAR_VAL | id
            | string  | bool      | null_ptr | val_init;
id        ::= ID_VAL;
string    ::= STR_VAL;
bool      ::= "true"  | "false";
null_ptr  ::= "null";
val_init  ::= "[" type "]" "{" [expr {"," expr} [","]] "}";

type      ::= (prim_type | id | pointer | array | ref | func) ["volatile"];
prim_type ::= "i8"  | "i16" | "i32"   | "i64" | "isize" | "u8"  | "u16"
            | "u32" | "u64" | "usize" | "f32" | "f64" | "bool";
pointer   ::= type ["var"] "*";
array     ::= type "[" expr "]";
ref       ::= type ["var"] "&";
func      ::= "(" [type {"," type}] ")" [":" type];
```

## Changelog

See [CHANGELOG.md](CHANGELOG.md)

## License

Copyright (C) 2020-2026 MaxXing. License GPLv3.
