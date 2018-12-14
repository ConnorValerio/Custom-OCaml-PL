# Custom-OCaml-PL
Custom-built programming language written in OCaml using ocamllex and ocamlyacc.

------------------------------------------------------------------------------------------
ABOUT - Authors: Connor Valerio & Ekrem Yurdakul
-----

This was a university assignment which required us to develop our own programming language using ocamllex & ocamlyacc. 
The language needed to be able to solve a number of pre-defined problems. 
In this repo you will find a number of example programs and a user guide that explains how the programming language works (e.g syntax, built-in functions & types)

The example program names are in the format pr<x>.spl (x: 1->10)

------------------------------------------------------------------------------------------
HOW TO RUN
----------

Use linux / Cygwin on Windows

Go to directory of the interpreter:

run command: make

run command: ./mysplinterpreter pr1.spl < input.txt

where 'pr1.spl' is the program written in our language and the 'input.txt' file contains the input for the program (each input should be separated by a newline character).

------------------------------------------------------------------------------------------
HOW IT WORKS:
-------------

-> mysplinterpreter takes the program as an argument from standard input and opens a lexer buffer containing the program code.

-> The lexer buffer is passed to lexer_main (in lexer.mll) where tokens are created based on the syntax of the language.

-> The tokens are passed to parser_main: expr EOF { $1 } (in parser.mly) where 'expr' is the whole program which is to be evaluated, EOF is an End of File token, and $1 is the first argument from standard input.

-> 'expr' is evaluated based on the 'expr' patterns below parser_main (i.e. 'expr' of type 'INT' evaluates to {LitI $1})

-> LitI stands for literal integer, and 'LitI $1' means the token LitI followed by the rest of the program (i.e. any expr). Similarly 'IF expr THEN expr ELSE expr' means the IF token, any expression, THEN token, any expresion, ELSE token, any expression.

-> Once the parser has parsed the program into expressions, the interpreter evaluates the parsed program.

-> The evaluation of the program happens in 'expr.ml' (let eval term = eval_helper [] [] term [];;).

-> evaluation happens by recursively calling eval_helper with the function environment, the argument environment and the term.

-> eval_helper matches the term with pre-defined expressions, for example, 'AddExpr($1,$3)' expression evaluates to:

(AddExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitI (x' + y')

-> This evaluates the $1 (x) and $3 (y) by checking if they can both be parsed into OCaml ints, this occurs in the method 'to_int_or_stuck()'.
-> Inside the 'to_int_or_stuck()' method, an exception is thrown if the value can not be parsed, but otherwise a new int is returned which is the sum of x' and y'.

-> The rest of the OCaml code written to assist the eval_helper, or to provide the language with other functions can be found at the top of the expr.ml file.
