%{
    open Expr
%}
%token <int> INT
%token <string> STRING
%token <string>CURLYLIST
%token <string>IDENT
%token DEF IN VAR READFILE CLISTREADIN INTREADIN STRINGTOINT STRINGREADIN
%token CAR CDR SORT MAKESET JOIN CONTAINS
%token IF THEN ELSE
%token LESSTHAN MORETHAN EQUALS
%token CONCAT FILTER LENGTH
%token ITYPE BTYPE CTYPE STYPE
%token APPEND GET PRINTLISTS
%token LPAREN RPAREN
%token TRUE FALSE
%token PLUS MINUS MULTIPLY DIVIDE
%token EOF
%start parser_main             /* the entry point */
%type <Expr.expr> parser_main
%%
parser_main: expr EOF { $1 }
;
expr: INT                                                {LitI $1}
	|STRING                                              {LitS $1}
	|TRUE                                                {LitB true}
	|FALSE												 {LitB false}	
	|CURLYLIST                                           {LitC $1}
	|IDENT                                               {Ident $1}
	|IF expr THEN expr ELSE expr                         {ConditionalExpr($2,$4,$6)}
	|expr PLUS expr                                      {AddExpr($1,$3)}
	|expr MINUS expr                                     {SubExpr($1,$3)}
	|expr LESSTHAN expr                                  {LessThanExpr($1,$3)}
	|expr MORETHAN expr                                  {MoreThanExpr($1,$3)}
	|expr EQUALS expr                                    {EqualExpr($1,$3)}
	|LPAREN expr RPAREN                                  {$2}
	|expr MULTIPLY expr                                  {MultiplyExpr($1,$3)}
	|expr DIVIDE expr 									 {DivideExpr($1,$3)}
	|expr LPAREN expr RPAREN                             {AppExpr($1,$3)}
	|CONCAT expr expr                                    {ConcatExpr($2,$3)}
	|APPEND expr expr                                    {AppendExpr($2,$3)}
	|GET expr expr                                       {GetExpr($2,$3)}
	|FILTER expr expr                                    {FilterExpr($2,$3)}
	|VAR IDENT EQUALS expr IN expr 					     {Var($2, $4, $6)} 										
	|DEF IDENT LPAREN IDENT RPAREN EQUALS expr IN expr   {Def($2,$4,$7,$9)}
	|CAR expr                                            {Car $2}
	|CDR expr                                            {Cdr $2}
	|LENGTH expr                                         {LengthExpr $2}
	|SORT expr 											 {SortExpr $2}
	|MAKESET expr                                        {MakeSetExpr $2}
	|JOIN expr expr 									 {JoinExpr($2, $3)}
	|PRINTLISTS expr expr 							     {PrintListsExpr($2,$3)}
	|CONTAINS expr expr 								 {ContainsExpr($2,$3)}
	|READFILE IN expr   							     {ReadfileExpr $3}
	|CLISTREADIN expr                                    {ReadinExprClist $2}
	|INTREADIN expr                                      {ReadinExprInt $2}
	|STRINGREADIN expr 									 {ReadinExprString $2}
	|STRINGTOINT expr 									 {StringtointExpr $2}
;
