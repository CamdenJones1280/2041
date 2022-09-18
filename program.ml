(* program.ml - a data structure for representing programs *)
exception TypeError of string
exception RuntimeError of string
let rt_err s = raise (RuntimeError s)
let type_err s = raise (TypeError s)

type expr =
  IntC of int | BoolC of bool
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Name of string
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Lt of expr * expr
  | Eq of expr * expr
  | Gt of expr * expr
  | Seq of expr list
  | While of expr * expr
  | Set of string * expr
  | Fun of string * expType * expr
  | Apply of expr * expr
  | Print of expr
  | Readint (*added for part 1*)
  | ListC of int list (*added part 3*)
  | Cons of expr * expr (*added part 3*)
  | Head of expr (*added part 3*)
  | Tail of expr (*added part 3*)
  | Funrec of string * string * expType * expType * expr (*added for part5 rec functions*)
 and expType = IntT | BoolT | UnitT | FunT of expType * expType | ListT
  (*added ListT above for part3*)

(* Type to represent a state of the program, e.g. the current stack of variables and the values they are bound to *)
type stType = (string * result) list
 (* Type to represent a value in the program *)
 and result = IntR of int | BoolR of bool | UnitR | Closure of expr*string*stType 
 | ListR of int list | ClosureRec of expr*string*string*stType
 (*added ListR above for part3*)
 (*added ClosureRec above for part5 rec functions*)

(* Searches the stack and updates the most recent binding with the new value *)
let rec assign name value state =
  match state with
  | [] -> rt_err "assign to unbound name"
  | (n,v)::t when n=name -> (name,value)::t
  | b::t -> b::(assign name value t)

(* pop a variable binding off the stack *)
let rec pop name state =
  match state with
  | [] -> rt_err "popping unbound name: internal error"
  | (n,v)::t when n=name -> t
  | b::t -> b::(pop name t)

(* evaluate an expression: return the value and the new program state *)
let rec eval exp state = match exp with
  | IntC n -> (IntR n, state)
  | BoolC b -> (BoolR b, state)
  | Add (e1,e2) -> evalInt (+) e1 e2 state
  | Mul (e1,e2) -> evalInt ( * ) e1 e2 state
  | Sub (e1,e2) -> evalInt (-) e1 e2 state
  | Div (e1,e2) -> evalInt (/) e1 e2 state
  | If (cond,thn,els) -> evalIf cond thn els state
  | Let (nm,vl,exp') -> evalLet nm vl exp' state
  | Name nm -> (List.assoc nm state, state)
  | And (e1,e2) -> evalBool (&&) e1 e2 state
  | Or (e1,e2) -> evalBool (||) e1 e2 state
  | Not e -> let (BoolR b, st') = eval e state in (BoolR (not b), st')
  | Lt (e1, e2) -> evalComp (<) e1 e2 state
  | Eq (e1, e2) -> evalComp (=) e1 e2 state
  | Gt (e1, e2) -> evalComp (>) e1 e2 state
  | Seq elist -> evalSeq elist state
  | While (cond,body) -> evalWhile cond body state
  | Set (name, e) -> let (vl, st') = eval e state in (UnitR, assign name vl st')
  | Fun (argname,_,body) -> (Closure (body,argname,state), state) (* "Captures" current environment at definition. *)
  | Apply (f,e) -> evalFunc f e state
  | Print e -> let (r,st') = eval e state in
	       let () = match r with
		 | UnitR -> print_string "()"
		 | IntR i -> print_int i
     | BoolR b -> print_string (if b then "True" else "False")
     | ClosureRec _ -> print_string "<funrec>"(*added for part5*)
		 | Closure _ -> print_string "<fun>" in
	       let () = print_string "\n" in
	       let () = flush stdout in
         (UnitR, st')
  | Readint -> evalReadInt state (*added for part 1 int inputs*)
  | ListC e -> (ListR e, state)(*added for part 3 lists*)
  | Head e -> evalHead e state (*added for part 3 lists*)
  | Tail e -> evalTail e state (*added for part 3 lists*)
  | Cons (e1,e2) -> evalCons (e1,e2) state(*added for part 3 lists*)
  | Funrec (f,x,_,_,e) -> (ClosureRec(e,f,x,state),state)(*added for part5 rec functions*)
and evalInt f e1 e2 state =
  let (IntR i1, st1) = eval e1 state in
  let (IntR i2, st2) = eval e2 st1 in
  IntR (f i1 i2), st2
and evalIf cond thn els state =
  let (BoolR b, st') = eval cond state in
  if b then eval thn st' else eval els st'
and evalLet name vl exp state =
  let (r, st') = eval vl state in
  let (r', st'') = eval exp ((name,r)::st') in
  (r', pop name st'')
and evalBool f e1 e2 state =
  let (BoolR b1, st1) = eval e1 state in
  let (BoolR b2, st2) = eval e2 st1 in
  BoolR (f b1 b2), st2
and evalComp cmp e1 e2 state =
  let (r1, st1) = eval e1 state in
  let (r2, st2) = eval e2 st1 in
  (BoolR (cmp r1 r2), st2)
and evalSeq elist st = match elist with (* Whee, tail recursion. *)
  | [] -> (UnitR, st)
  | e::[] -> eval e st
  | e::t -> let (_, st') = eval e st in
	    evalSeq t st'
and evalWhile cond body st = (* Note the tail recursion. An infinite while loop won't blow the stack *)
  let (BoolR b, st') = eval cond st in
  if (not b) then (UnitR, st') else
    let (_, st'') = eval body st' in
    evalWhile cond body st''
and evalFunc f arg state = (* Note: we need to evaluate the function with environment at time of definition *)
  match (eval f state) with (*added match statement to pick between normal and rec functions*)
  | (Closure (body,argname,def_st), s) -> 
    let (Closure (body,argname,def_st), st') = eval f state in
    let (argval, st'') = eval arg st' in (* but computing its argument could change state at call site *)
    let (result, _) = eval body ((argname,argval)::def_st) in
    (result, st'') (* So state after call must be the state after argument computation *)
    (*added closurerec for part5 and adds fname and closurerec to state*)
  | (ClosureRec (body,fname,argname,def_st), s) -> 
    let (ClosureRec(body,fname,argname,def_st), st')= eval f state in
    let (argval, st'') = eval arg st' in 
    let (result, _) = eval body ((fname, ClosureRec(body,fname,argname,def_st))::(argname,argval)::def_st) in
    (result, st'')
and evalReadInt state =
  (IntR (read_int()), state) (*added for part 1*)
and evalHead e state = 
  match (eval e state) with 
  | (ListR l, st) -> (IntR (List.hd l), st) (*added for part3*)
and evalTail e state =
  match (eval e state) with 
  | (ListR l, st) -> (ListR (List.tl l), st) (*added for part3*)
and evalCons (e1,e2) state =
    match (eval e1 state) with 
    | (IntR e, st1) -> match (eval e2 st1) with 
      | (ListR l, st2) -> (ListR(e::l),st2)(*added for part3*)

(* Type checking/inference: Figure out type for an expression.  Fail if the expression is not well-typed.*)
let rec typeof exp env = match exp with
  | IntC _ -> IntT
  | BoolC _ -> BoolT
  | Add (e1,e2) | Sub (e1,e2) | Mul (e1,e2)
  | Div (e1,e2) ->
     ( match (typeof e1 env, typeof e2 env) with
       | (IntT,IntT) -> IntT
       | _ -> type_err "Arithmetic on non-integer arguments")
  | And (e1,e2)
  | Or (e1,e2) ->
     ( match (typeof e1 env, typeof e2 env) with
       | (BoolT,BoolT) -> BoolT
       | _ -> type_err "Boolean operation on non-Bool arguments")
  | Not e -> if (typeof e env) = BoolT then BoolT else type_err "Not of non-Boolean"
  | Lt (e1,e2)
  | Gt (e1,e2) ->
     ( match (typeof e1 env, typeof e2 env) with
       | (IntT,IntT) -> BoolT
       | _ -> type_err "Comparison of non-integer values" )
  | Eq (e1,e2) ->
     ( match (typeof e1 env, typeof e2 env) with
       | (IntT,IntT) | (BoolT,BoolT) | (UnitT,UnitT) | (ListT, ListT) -> BoolT (*added (ListT, ListT) for part3*)
       | _ -> type_err "Equality test on incompatible values" )
  | If (cond,thn,els) ->
     if not ((typeof cond env) = BoolT) then type_err "If on non-boolean condition" else
       let (t1,t2) = (typeof thn env, typeof els env) in
       if (t1 = t2) then t1 else type_err "Different types for then/else branches"
  | Name name -> (try List.assoc name env with Not_found -> type_err ("Unbound variable "^name))
  | Let (name,vl,e) ->
     let t = typeof vl env in
     typeof e ((name,t)::env)
  | Seq elist -> seqType elist env
  | While (c,body) ->
     ( match (typeof c env, typeof body env) with
       | (BoolT, _) -> UnitT
       | _ -> type_err "Non-boolean condition for while")
  | Set (name, e) -> if (typeof (Name name) env) = (typeof e env) then UnitT else type_err "assign type mismatch"
  | Fun (argname, argType, body) ->
     let resType = typeof body ((argname,argType)::env) in
     FunT (argType,resType)
  | Apply (e1,e2) ->
     ( match (typeof e1 env) with
       | FunT (argtype, restype) -> if (typeof e2 env) = argtype then restype
				       else type_err "incompatible function argument"
       | _ -> type_err "Apply of non-function value")
  | Print e -> let _ = typeof e env in UnitT
  | Readint -> IntT (*added for part1*)
  | ListC _ -> ListT (*added for part3*)
  | Head e -> (match (typeof e env) with | ListT -> IntT | _ -> type_err "Head") (*added for part 3 *)
  | Tail e -> (match (typeof e env) with | ListT -> ListT | _ -> type_err "Tail") (*added for part 3, e : ListT => Tail e : ListT*)
  | Cons (e1,e2) -> if ((typeof e1 env) = IntT) && ((typeof e2 env) = ListT) 
      then ListT else type_err "Cons"(*added for part 3, e1 : IntT, e2 : ListT => Cons (e1,e2) : ListT becasue the rest is a LisT*)
  | Funrec(f,x,t2,t1,e) -> let recResType = (typeof e ((f,FunT(t1,t2))::(x,t1)::env)) 
      in (if recResType = t2 then FunT(t1,t2) else type_err "Funrec error")
   (*added Funrec for rec function type checking*) 
  and seqType el env = match el with
  | [] -> UnitT
  | [e] -> typeof e env
  | e::rest -> let _ = typeof e env in seqType rest env
