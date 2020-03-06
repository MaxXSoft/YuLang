# Yu

Yu (羽) is a simple system programming language.

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
declare   ::= property "declare" id ":" type;
ty_alias  ::= visib "type" id "=" type;
struct    ::= visib "struct" id "{" arg_list [","] "}";
enum      ::= visib "enum" id [":" type] "{" enum_list "}";
import    ::= visib "import" id {"." id};

property  ::= ["public" | "extern"];
visib     ::= ["public"];
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
unary     ::= [unary_op | "sizeof"] factor;
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
