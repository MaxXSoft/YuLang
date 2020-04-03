# Yu

![YuLang](YuLang.png)

Yu (羽) is a simple system programming language.

## Building from Source

Before building YuLang compiler, please make sure you have installed the following dependencies:

* `cmake` 3.5 or later
* `llvm` 8.0 or later

Then you can build this repository by executing the following command lines:

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
            | enum    | import  | if_else | when      | while
            | for_in  | asm     | control | expr;

if_else   ::= "if" expr block ["else" (if_else | block)];
when      ::= "when" expr "{" when_elem {when_elem} ["else" block] "}";
while     ::= "while" expr block;
for_in    ::= "for" id "in" expr block;
asm       ::= "asm" "{" string {string} "}";
control   ::= "break" | "continue"  | ("return" [expr]);

when_elem ::= expr {"," expr} block;

expr      ::= binary {id binary};
binary    ::= cast {bin_op cast};
cast      ::= unary ["as" type];
unary     ::= [unary_op factor | "sizeof" type];
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
prim_type ::= "i8"  | "i16" | "i32" | "i64" | "u8"  | "u16"
            | "u32" | "u64" | "f32" | "f64" | "bool";
pointer   ::= type ["var"] "*";
array     ::= type "[" expr "]";
ref       ::= type ["var"] "&";
func      ::= "(" [type {"," type}] ")" [":" type];
```

## License

Copyright (C) 2010-2020 MaxXing. License GPLv3.
