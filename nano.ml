exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup: string * env -> value
 * given environment and the variable name and return the value 
 *)
let lookup (x,evn) = 
    let result = listAssoc (x,evn) in
    match result with 
        | None -> raise (MLFailure ("variable not bound: " ^ x)) (* if not exist *)
        | Some v -> v

(* eval: env * expr -> value   
 * given environment and a expression and return the value of the expression
 *)
let rec eval (evn,e) = match e with
    | Const c -> Int c                (* for constant *)
    | True -> Bool true               (* for boolean *)
    | False -> Bool false 
    | Var v -> lookup (v,evn)         (* for variable *)
    | Bin (e1,bop,e2) -> let m =      (* for binary operation *)
        let eval1 = eval (evn,e1) in  (* two recursive calls *)
        let eval2 = eval (evn,e2) in
        match bop with                (* switch cases on binary operator *)
            | Plus -> let m = match (eval1, eval2) with (Int i1, Int i2) -> Int (i1 + i2)
                      | (_, _) -> raise (MLFailure "unmatched type for(+)") in m
            | Minus -> let m = match (eval1, eval2) with (Int i1, Int i2) -> Int (i1 - i2)
                       | (_, _) -> raise (MLFailure "unmatched type for (-)") in m
            | Mul -> let m = match (eval1, eval2) with (Int i1, Int i2) -> Int (i1 * i2)
                     | (_, _) -> raise (MLFailure "unmatched type for (*)") in m
            | Div -> let m = match (eval1, eval2) with (Int i1, Int i2) -> Int (i1 / i2) 
                     | (_, _) -> raise (MLFailure "unmatched type for (/)") in m
            | Eq -> let m = match (eval1, eval2) with
                        | (Bool b1, Bool b2) -> Bool (b1 = b2)
                        | (Int i1, Int i2) -> Bool (i1 = i2)
                        | (_, _) -> raise (MLFailure "unmatched type for comparison")
                    in m
            | Ne -> let m = match (eval1, eval2) with
                        | (Bool b1, Bool b2) -> Bool (b1 <> b2)
                        | (Int i1, Int i2) -> Bool (i1 <> i2)
                        | (_, _) -> raise (MLFailure "unmatched type for comparison")
                    in m
            | Lt -> let m = match (eval1, eval2) with 
                        | (Int i1, Int i2) -> Bool (i1 < i2)
                        | (_, _) -> raise (MLFailure "required type int")
                    in m
            | Le -> let m = match (eval1, eval2) with
                        | (Int i1, Int i2) -> Bool (i1 <= i2)
                        | (_, _) -> raise (MLFailure "required type int")
                    in m
            | And -> let m = match (eval1, eval2) with
                         | (Bool b1, Bool b2) -> Bool (b1 && b2)
                         | (_, _) -> raise (MLFailure "required type boolean")
                     in m
            | Or -> let m = match (eval1, eval2) with
                        | (Bool b1, Bool b2) -> Bool (b1 || b2)
                        | (_, _) -> raise (MLFailure "required type boolean")
                    in m
        in m
    | If (p,t,f) -> let m = let p' = eval (evn,p) in (* first evalutate p *)
                            match p' with            (* then evaluate t and f *)
                                | Bool b -> if b then eval (evn,t) else eval (evn,f)
                                | _ -> raise (MLFailure "required type boolean")
                    in m
    | Let (s,e1,e2) -> let v = eval (evn,e1) in (* first evaluate e1 *)
                       let evn' = (s,v)::evn in (* then insert it temporarily into a new environment *)
                       eval (evn',e2)
    | Letrec (s,e1,e2) -> let v = match e1 with Fun (x,e) -> Closure (evn, Some s, x, e)
                                  | _ -> raise (MLFailure "required type function") in
                          let evn' = (s,v)::evn in
                          eval (evn',e2) (* similarly but this time for function only *)
    | Fun (x,e) -> Closure (evn, None, x, e)                   
    | App (e1,e2) -> let Closure (evn', name, x, e) = eval (evn,e1) in
                     let x' = eval (evn,e2) in
                     match name with None -> eval ((x,x')::evn',e) (* two cases: recursive/non-recursive *)
                     | Some s -> eval ((s, Closure (evn',name,x,e))::(x,x')::evn',e)
                     (* if recursive, insert the function itself into the new environment *)


(*********************     Testing Code  ******************************)
