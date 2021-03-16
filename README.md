# Yu

![YuLang](YuLang.png)

[![Build and Test](https://github.com/MaxXSoft/YuLang/workflows/Build%20and%20Test/badge.svg)](https://github.com/MaxXSoft/YuLang)
[![羽语言简明教程](https://img.shields.io/badge/tutorial-%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87-blue)](https://maxxsoft.github.io/YuLang-doc/tutorial/zh-cn/)

Yu (羽) is a simple system programming language.

## Documentations

* Tutorial: [简体中文版 (GitHub)](https://maxxsoft.github.io/YuLang-doc/tutorial/zh-cn/), [简体中文版 (Gitee)](https://maxxsoft.gitee.io/yulang-doc/tutorial/zh-cn/).

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

* `cmake` 3.13 or later
* `llvm` 8.0 or later
* C++ compiler supporting C++17

You may want to check the toolchain configuration in `toolchain.mk`. Then you can build this repository by executing the following command lines:

```
$ git clone --recursive https://github.com/MaxXSoft/YuLang.git
$ cd YuLang
$ mkdir build
$ cd build
$ cmake .. && make -j8
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

Copyright (C) 2010-2020 MaxXing. License GPLv3.
