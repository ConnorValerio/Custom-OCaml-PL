type typeExpr = ITy | BTy


type expr = 
    | Ident of string
    | LitI of int
    | LitB of bool
    | LitS of string
    | LitC of string
    | Def of string * string * expr * expr
    | AppExpr of expr * expr
    | AddExpr of expr * expr
    | SubExpr of expr * expr
    | LessThanExpr of expr * expr
    | MoreThanExpr of expr * expr
    | EqualExpr of expr * expr
    | ConditionalExpr of expr * expr * expr
    | ConcatExpr of expr * expr
    | AppendExpr of expr * expr
    | GetExpr of expr * expr
    | FilterExpr of expr * expr
    | Var of string * expr * expr
    | Car of expr
    | Cdr of expr
    | LengthExpr of expr
    | MultiplyExpr of expr * expr
    | DivideExpr of expr * expr
    | SortExpr of expr
    | MakeSetExpr of expr
    | JoinExpr of expr * expr
    | PrintListsExpr of expr * expr
    | ContainsExpr of expr * expr
    | ReadfileExpr of expr
    | ReadinExprClist of expr 
    | ReadinExprInt of expr
    | ReadinExprString of expr 
    | StringtointExpr of expr

    

val eval : expr -> expr

val print_res : expr -> unit
