(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule lexer_main = parse
      [' ' '\t' '\n']               { lexer_main lexbuf }     (* Takes no notice of whitespace *)
    | ['0'-'9']+ as lxm             { INT(int_of_string lxm) }
    | '{'['a'-'z' ',' ':' ' ']*'}' as lxm  { CURLYLIST(lxm) }
    | '$'['a'-'z' '0'-'9']+ as lxm          {IDENT(lxm)}
    | "Bool"                        { BTYPE }
    | "Int"                         { ITYPE }
    | "String"                      { STYPE }
    | "CurlyList"                   { CTYPE }  
    | "DEFINE"                      { DEF }
    | "IN"                          { IN }
    | "IF"                          { IF }
    | "THEN"                        { THEN }
    | "ELSE"                        { ELSE }
    | "CAR"                         { CAR }
    | "CDR"                         { CDR }
    | "true"                        { TRUE }
    | "false"                       { FALSE }   
    | ['a'-'z']+ as lxm             { STRING(lxm) }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | '<'                           { LESSTHAN }
    | '>'                           { MORETHAN }
    | '('                           { LPAREN }
    | ')'                           { RPAREN }
    | '='                           { EQUALS }
    | '*'                           { MULTIPLY }
    | '/'                           { DIVIDE }
    | eof                           { EOF }
    | "FILTER"                      { FILTER }
    | "APPEND"                      { APPEND }
    | "GET"                         { GET }
    | "CONCAT"                      { CONCAT }
    | "VAR"                         { VAR }
    | "LENGTH"                      { LENGTH }
    | "SORT"                        { SORT }
    | "SETIFY"                      { MAKESET }
    | "JOIN"                        { JOIN }
    | "PRINTLISTS"                  { PRINTLISTS }
    | "CONTAINS"                    { CONTAINS }
    | "INTREADIN"                   { INTREADIN }
    | "CLISTREADIN"                 { CLISTREADIN }
    | "READFILE"                    { READFILE }
    | "STRING_TO_INT"               { STRINGTOINT }
    | "STRINGREADIN"                { STRINGREADIN }

 