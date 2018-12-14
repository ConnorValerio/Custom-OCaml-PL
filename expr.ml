open Printf
let rev lst =
    let rec aux acc = function
    |[] -> acc
    |h :: t -> aux (h :: acc) t in
aux [] lst 

let rec join_lists lst1 lst2 = match lst2 with | [] -> lst1 | h :: t -> join_lists (h :: lst1) t;;

let getString lst = 
    let rec aux str = function
    |[] -> str
    |h :: t -> aux (str^(String.make 1 h)) t in
    (aux "" lst)

let removeBlanks strlst =
    let rec aux acc = function
    |[] -> acc
    |h :: t -> if h=' ' || h='\t' then aux acc t else aux (h :: acc) t in   
    aux [] strlst

let converterChar str =
    let rec aux n acc =
    match str.[n] with
    | '{' -> aux (n + 1) acc
    | ',' -> aux (n + 1) (str.[n] :: acc)
    | '}' -> acc
    | _   -> aux (n + 1) (str.[n] :: acc) in
    removeBlanks ( aux 0 [])

let getInputForm lst =
    let rec aux str = function
    |[] -> "{"^str^"}"
    |h :: [] -> aux (str^h) []
    |h :: t -> aux (str^h^",") t in
    aux "" lst

let converterString2 charList =
    let rec aux acc acc2 = function
    |[]  -> []
    |[x] -> (getString (x :: acc2)) :: acc
    |a :: (b :: _ as t) -> if ( b=',') then aux ((getString(a :: acc2)) :: acc) [] t else if (a=',') then aux acc acc2 t else aux acc (a :: acc2) t in
    aux [] [] (rev charList)

let appendFunc elem lst =
    getInputForm (List.append (converterString2(converterChar lst)) (elem :: []) )

let filter0 lst k = 
    let rec aux acc count = function 
    | [] -> acc 
    | h :: t -> if (count < k) then aux (h :: acc) (count + 1) t else acc in aux [] 0 lst

let filter lst k = 
    getInputForm (rev (filter0 (converterString2 (converterChar lst)) k))

let generateList str = (converterString2(converterChar str))

exception IndexOutOfBounds;;

let get_nth lst nth =
    let rec aux count = function
    | [] -> "IndexOutOfBounds"
    | h :: t -> if(nth = count) then h else aux (count+1) t in

    aux 1 lst;;

let car str =
    let aux = function
    |[] -> ""
    |h :: t -> h in
    aux (generateList str);;

let cdr str =
    let aux = function
    |[] -> []
    |h :: t -> t in
    aux (generateList str);;

let length str =
    let rec aux cnt = function    
    |[] -> cnt
    |h :: t -> aux (cnt+1) t in
    aux 0 (generateList str)

let rec contains lst elem =
 match lst with
  |[] -> false 
  |h :: t -> if (elem = h) then true else contains t elem ;;

let makeSet lst =
 let rec aux acc = function
  |[] -> acc 
  |h :: t -> if(contains acc h) then aux acc t else aux (h :: acc) t 
in rev (aux [] lst);;

let rec sortaux = function
|[] -> []
|h :: t -> insert h (sortaux t)
and insert elem = function
|[] -> [elem]
| h :: t -> if elem < h then elem :: h :: t
            else h :: insert elem t;;
let sort str =
    getInputForm (sortaux (generateList(str)));;

let input_line_aux inchan =
  try Some (input_line inchan)
  with End_of_file -> None ;;

let readfile chan =
    let rec aux acc =
    match input_line_aux chan with
    | Some (line) -> aux (line :: acc)
    | None -> begin
        close_in chan;
        (acc)
    end in
    aux [];;

let stringToInt str =
    int_of_string str;;



type typeExpr = ITy 
    | BTy

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


let rec lookup env v = match env with 
    | [] -> failwith ("cannot find var: " ^ v)
    | (vname, vvalue) :: rest -> if v = vname 
                                     then vvalue
                                     else lookup rest v

exception InvalidTypeError of string

let rec eval_helper func_env arg_env term inputs = 

    let to_int_or_stuck_single(x)= 
        let xEval = eval_helper func_env arg_env x inputs
        in (match xEval with
              | LitI x'-> x'
              | _ -> raise (InvalidTypeError "String Parse Error (Single): Unable to parse the Strings supplied.")) in

    let to_int_or_stuck(x, y) = 
        let xEval = eval_helper func_env arg_env x inputs
        and yEval = eval_helper func_env arg_env y inputs
        in (match (xEval, yEval) with
              | (LitI x', LitI y') -> (x', y')
              | _ -> raise (InvalidTypeError "Mathematical Operation: Requires two Integer types")) in

    let to_string_or_stuck(x, y) =
        let xEval = eval_helper func_env arg_env x inputs
        and yEval = eval_helper func_env arg_env y inputs
        in (match (xEval, yEval) with
              | (LitS x', LitS y') -> (x', y')
              | _ -> raise (InvalidTypeError "CONCAT: Two Strings expected for this function.")) in

    let to_string_or_stuck_single(s) =
        let sEval = eval_helper func_env arg_env s inputs
        in (match sEval with
              | LitS s' -> s'
              | _ -> raise (InvalidTypeError "String Parse Error (Single): Unable to parse the Strings supplied.")) in

    let to_clist_or_stuck(l) =
        let lEval = eval_helper func_env arg_env l inputs
        in (match lEval with
              |LitC x' -> x'
              |_ -> raise (InvalidTypeError "CList Parse Error: Unable to parse the CList supplied."))

    in match term with
        | (Ident v) -> lookup arg_env v
        | (LitI i) -> LitI i
        | (LitB b) -> LitB b
        | (LitC c) -> LitC c
        | (LitS s) -> LitS s
        | (ConditionalExpr (cond, tExpr, fExpr)) -> 
            let condEval = eval_helper func_env arg_env cond inputs
            in (match condEval with
                  | (LitB b) -> 
                      eval_helper func_env arg_env (if b then tExpr else fExpr) inputs
                  | _ -> raise (InvalidTypeError "IF STATEMENT: Conditional Statement is not of type Boolean."))

        | (AddExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitI (x' + y')
        | (SubExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitI (x' - y')
        | (MoreThanExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitB (x' > y')
        | (LessThanExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitB (x' < y')
        |ConcatExpr(str1,str2)->
            let (str1',str2') = to_string_or_stuck (str1, str2)
            in if (str1' = ":") then LitS(str2') else LitS(str1' ^ str2')
        |AppendExpr(str,clist)->
            let str' = to_string_or_stuck_single str
            and clist' = to_clist_or_stuck clist in 
            LitC (appendFunc str' clist')
        |FilterExpr(clist, limiter)->
            let clist' = to_clist_or_stuck clist
            and limiter' = to_int_or_stuck_single limiter in
            LitC (filter clist' limiter')        
        |GetExpr(clist,index)->
            let clist' =  to_clist_or_stuck clist
            and index' =  to_int_or_stuck_single index in
            LitS (get_nth (generateList clist') index')
        | Car(clist) ->
            let clist' = to_clist_or_stuck clist in
            LitS (car clist') 
        | Cdr(clist)->
            let clist' = to_clist_or_stuck clist in
            LitC (getInputForm (cdr clist'))
        | ReadfileExpr(exp) ->
            eval_helper func_env arg_env exp (rev (readfile stdin))
        | ReadinExprClist(linenumber) ->
            let linenumber' = to_int_or_stuck_single linenumber in
            LitC (get_nth inputs linenumber')
        | ReadinExprInt(linenumber) ->
            let linenumber' = to_int_or_stuck_single linenumber in
            LitI (stringToInt(get_nth inputs linenumber'))
        | ReadinExprString(linenumber) ->
            let linenumber' = to_int_or_stuck_single linenumber in
            LitS (get_nth inputs linenumber')    
        | StringtointExpr(str) ->
            let str' = to_string_or_stuck_single str in
            LitI (stringToInt str')    
        |MultiplyExpr(x,y)->
            let(x',y') = to_int_or_stuck(x, y)
            in LitI(x' * y')
        |DivideExpr(x,y)->
            let(x',y') = to_int_or_stuck(x, y)
            in LitI(x' / y')    
        |LengthExpr(clist)->
            let clist' = to_clist_or_stuck clist in
            LitI(length (clist'))  
        |SortExpr(clist)->
            let clist' = to_clist_or_stuck clist in
            LitC(sort (clist'))
        |ContainsExpr(clist, elem)->
            let clist' = to_clist_or_stuck clist
            and elem' = to_string_or_stuck_single elem in
            LitB (contains (generateList clist') elem')   
        |MakeSetExpr(clist)->
            let clist' = to_clist_or_stuck clist in
            LitC(getInputForm (makeSet (generateList(clist'))))
        | JoinExpr(clist1, clist2) ->  
            let clist1' = to_clist_or_stuck clist1 
            and clist2' = to_clist_or_stuck clist2 in
            LitC (getInputForm (join_lists (generateList clist1') (generateList clist2')))
        |PrintListsExpr(clist1,clist2)->
            let clist1' = to_clist_or_stuck clist1 
            and clist2' = to_clist_or_stuck clist2 in
            LitS(clist1'^"\n"^clist2')                 
        | Var (name,body,inExpr) ->
           let eval = eval_helper func_env arg_env body inputs in
           eval_helper func_env ((name, eval) :: arg_env) inExpr inputs
        | Def (name, argName, body, inExpr) ->
            eval_helper ((name, (argName, body)) :: func_env) arg_env inExpr inputs  
        | EqualExpr(var1,var2)->
            let var1Eval = eval_helper func_env arg_env var1 inputs
            and var2Eval = eval_helper func_env arg_env var2 inputs in
            (match (var1Eval,var2Eval) with
                | (LitS(var1'),LitS(var2')) -> LitB (to_string_or_stuck_single(var1) = to_string_or_stuck_single(var2))
                | (LitI(var1'),LitI(var2')) -> LitB (to_int_or_stuck_single(var1) = to_int_or_stuck_single(var2))
                | (LitC(var1'),LitC(var2'))  -> LitB (to_clist_or_stuck(var1) = to_clist_or_stuck(var2))
                | (Ident(var1'),LitC(var2'))  -> LitB (to_clist_or_stuck(var1) = to_clist_or_stuck(var2)))

        | AppExpr (func, arg) -> 
            let argEval = eval_helper func_env arg_env arg inputs
            in (match func with
                | (Ident f) -> 
                    (match lookup func_env f with
                        | (argName, body) -> 
                            eval_helper func_env ((argName, argEval) :: arg_env) body inputs) 
                | _ -> raise (InvalidTypeError "Application Expression: Function was not supplied."))
                 
                

let eval term = eval_helper [] [] term [];;

exception NonBaseTypeResult

let print_res res = match res with
    | (LitS s) -> print_string s
    | (LitC c) -> print_string c
    | (LitI i) -> print_int i
    | (LitB b) -> print_string (if b then "true" else "false")
    | _ -> raise NonBaseTypeResult;;

