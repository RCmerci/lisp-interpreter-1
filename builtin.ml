open Type

let top_env = {
    varmap=Util.StrMap.empty;
    fmmap =Util.StrMap.empty;
    upper_env=None;
  };;
(* utils *)
let aux_set (name:string) (value:value) = Env.set_fm name value top_env
let nil2false a =
  match a with
  | Vwarp (Slist []) -> Vwarp (Bool false)
  | _ -> a
let bool2value b = Vwarp (Bool b)
let value2bool v =
  match v with
  | Vwarp (Bool b) -> b
  | _ -> raise (Failure ("impossible"^(string_of_int __LINE__)))
let unwarp_Vfunc v =
  match v with
  | Vfunc f -> f
  | _ -> raise (Error.wrong_type_arg_error "unwarp_Vfunc")
(* utils *)

let defun (args:sexp list) (env:environ) : sexp =
  let func_name = List.hd args |> Util.sym2str in
  let args' = List.tl args |> List.hd in
  let body = List.tl args |> List.tl in
  Vfunc (Eval.define_func
	   func_name
	   (Util.slist2string_list args')
	   body
	   env)
  |> Env.set_fm ~env:env func_name;
  Slist []

let lambda (arg : sexp list) (env:environ) : value =
  let args' = List.hd arg in
  let body = List.tl arg in
  Vfunc (Eval.define_func
	   "_lAmDBdA_"
	   (Util.slist2string_list args')
	   body
	   env)

let defmacro (args:sexp list) (env:environ) : sexp =
  let macro_name = List.hd args |> Util.sym2str in
  let args' = List.tl args |> List.hd in
  let body = List.tl args |> List.tl in
  Vmacro (Eval.define_macro
	    macro_name
	    (Util.slist2string_list args')
	    body
	    env
	 )
  |> Env.set_fm ~env:env macro_name;
  Slist []

let car (arg: value list) : value =
  if List.length arg <> 1 then raise (Error.wrong_number_of_arguments "car") else
    match List.hd arg with
    | Vwarp (Slist []) -> nil
    | Vwarp (Slist (hd :: _)) -> Vwarp hd
    (* | Vwarp (DotSlist ([], s)) -> Vwarp s *)    (*   (car '( . 2))  是错的 *)
    | Vwarp (DotSlist (hd::_tl, _s)) -> Vwarp hd
    | _ -> raise (Error.Wrong_type_arg "car")

let cdr (arg: value list) : value =
  if List.length arg <> 1 then raise (Error.wrong_number_of_arguments "cdr") else
    match List.hd arg with
    | Vwarp (Slist []) -> nil
    | Vwarp (Slist (_ :: tl)) -> Vwarp (Slist tl)
    | Vwarp (DotSlist (_ :: tl, s)) -> Vwarp (DotSlist (tl, s))
    | _ -> raise (Error.wrong_type_arg_error "cdr")

let cons (arg : value list) : value =
  match arg with
  | [Vwarp car_; Vwarp((Number _)as x) ]
  | [Vwarp car_; Vwarp((String _)as x) ]
  | [Vwarp car_; Vwarp((Bool _)  as x) ] 
  | [Vwarp car_; Vwarp((Symbol _)as x) ] -> Vwarp (DotSlist ([car_], x))
  | [Vwarp car_; Vwarp (Slist x)] -> Vwarp (Slist (car_ :: x))
  | [Vwarp car_; Vwarp (DotSlist (sl, s))] -> Vwarp (DotSlist (car_::sl, s))
  | _ -> raise (Error.wrong_number_of_arguments "cons")
	       
		 
let add (arg : value list) : value =
  let args' = arg |> List.map (function 
				| Vwarp Number (n:int) ->  n
				(* | Vwarp Number (n:float)       -> n *)  (*暂时只有int，没有float。 见 type.ml*)
				| _ -> raise (Error.wrong_type_arg_error "add")
			      ) 
  in
  match List.length args' with
  | 0 -> raise (Error.wrong_number_of_arguments "add")
  | 1 -> Vwarp (Number (List.hd args'))
  | _ -> 
     Vwarp (Number (List.fold_left (+) 0 args'))

let sub (arg : value list) : value =
  let args' = arg |> List.map (function
				| Vwarp Number (n:int) -> n
				| _ -> raise (Error.wrong_type_arg_error "sub")
			      )
  in
  match List.length args' with
  | 0 -> raise (Error.wrong_number_of_arguments "sub")
  | 1 -> Vwarp (Number (List.hd args'|> (-) 0))
  | _ ->
     Vwarp (Number (List.fold_left (-) (List.hd args') (List.tl args')))

let mul (arg: value list) : value =
  let args' = arg |> List.map (function
				| Vwarp Number (n:int) -> n
				| _ -> raise (Error.wrong_type_arg_error "mul")
			      )
  in
  match List.length args' with
  | 0 -> raise (Error.wrong_number_of_arguments "sub")
  | 1 -> Vwarp (Number (List.hd args'))
  | _ ->
     Vwarp (Number (List.fold_left ( * ) 1 args'))

let div (arg :value list) : value =
  let args' = arg |> List.map (function
				| Vwarp Number (n:int) -> n
				| _ -> raise (Error.wrong_type_arg_error "div")
			      )
  in
  match List.length args' with
  | 0 -> raise (Error.wrong_number_of_arguments "sub")
  | 1 -> Vwarp (Number (List.hd args'))
  | _ ->
     try Vwarp (Number (List.fold_left (/) (List.hd args') (List.tl args'))) with
     | Division_by_zero -> raise Error.div_zero_error


let quote (arg : sexp list) _env : value =
  if List.length arg <> 1
  then raise (Error.wrong_number_of_arguments "quote") else
    Vwarp (List.hd arg)

let list (arg : value list) : value =
  if 0 = List.length arg then nil else
    Vwarp (Slist (List.map
		    (function Vwarp s -> s
			    | Vfunc f -> Swarp f
			    | _ -> raise (Failure ("impossible "^(string_of_int __LINE__ ))))
			   arg))
      
  
(* let quasiquote (arg : sexp list) (env : environ): value = *)
(*   if List.length arg <> 1 *)
(*   then raise (Error.wrong_number_of_arguments "quasiquote") else *)
(*     Vwarp *)
(*       (Eval.scan_and_eval_unquote (List.hd arg) env) *)


let quasiquote (arg : sexp list) : sexp =
  if List.length arg <> 1
  then raise (Error.wrong_number_of_arguments "quasiquote") else
    let aux = function
      | Number _ as x -> x
      | String _ as x -> x
      | Bool _ as x -> x
      | DotSlist _ as x -> x
      | Slist [] as x -> x
      | Slist [Symbol "unquote"; x] -> x
      | Slist _ as x -> Slist [Symbol "quote"; x]
      | Symbol _ as x -> Slist [Symbol "quote"; x]
      | Swarp f -> raise (Failure ("impossible:builtin:"^(string_of_int __LINE__)))
    in
    let arg' = match (List.hd arg) with
      | Slist l -> l
      | _ -> raise (Failure ("impossible "^(string_of_int __LINE__)))
    in
    Slist ((Symbol "list") :: List.map aux arg')

let set (arg : value list) (env:environ) : value =
  if List.length arg <> 2 then raise (Error.wrong_number_of_arguments "set") else
    let name = List.hd arg
    and value = List.tl arg |> List.hd in
    match name with
    | Vwarp (Symbol name') -> Env.set_var name' value env; value
    | _ -> raise (Error.wrong_type_arg_error "set")

let _let (arg : sexp list) (env:environ) : value =
  let env' = Env.new_env env in
  let rec bind_aux bindl =
      match bindl with
      | [] -> nil
      | (Slist [(Symbol _) as sym1; v]) :: tl ->
	   let _ = set [(Vwarp sym1); Eval.eval v env'] env' in
	   bind_aux tl
      | _ -> raise (Error.wrong_type_arg_error "let")
  in
  match arg with
  | (Slist bindl) :: tl_body -> (
    let _ = bind_aux bindl in
    Eval.eval_sexp_list tl_body env')
  | [] -> raise (Error.wrong_number_of_arguments "let")
  | _ -> raise (Failure ("impossible : builtin :" ^ (string_of_int __LINE__)))
	       
let equal (arg : value list) : value =
  if List.length arg <> 2 then raise (Error.wrong_number_of_arguments "equal") else
  let rec aux arg = 
    match arg with
    | [Vwarp (Number n1); Vwarp (Number n2)] -> n1 = n2
    | [Vwarp (String s1); Vwarp (String s2)] -> s1 = s2
    | [Vwarp (Bool   b1); Vwarp (Bool   b2)] -> b1 = b2
    | [Vwarp (Symbol s1); Vwarp (Symbol s2)] -> s1 = s2
    | [Vwarp (Slist  l1); Vwarp (Slist l2)] ->
       (try List.exists2 (fun a b -> aux [Vwarp a;Vwarp b] |> not) l1 l2 |> not with
       | Invalid_argument "List.exists2" -> false)
    | [Vwarp (DotSlist (l1, s1)); Vwarp (DotSlist (l2, s2))] ->
       let rst =
	 try List.exists2 (fun a b -> aux [Vwarp a;Vwarp b] |> not) l1 l2 with
	 | Invalid_argument "List.exists2" -> true
       in
       if rst then not rst else
	 aux [Vwarp s1;Vwarp s2]
    | [Vwarp (Slist []); Vwarp (Bool false)] -> true
    | [Vwarp (Bool false); Vwarp (Slist [])] -> true
    | [Vfunc f1; Vfunc f2] -> raise (Error.wrong_type_arg_error "equal")
    | _ -> false
  in
  aux arg |> bool2value
	 
let _if (arg :sexp list) (env: environ) : value =
  if List.length arg < 2 then raise (Error.wrong_number_of_arguments "if") else
    match arg with
    | cond :: f_stat :: t_statl ->
       let evalrst = Eval.eval cond env |> nil2false in
       let eqrst =
	 match equal [evalrst; Vwarp (Bool false)] with
	 | Vwarp (Bool a) -> a
	 | _ -> raise (Failure ("impossible"^(string_of_int __LINE__)))
       in
       if eqrst then
	 Eval.eval_sexp_list t_statl env
       else Eval.eval f_stat env
    | _ -> raise (Failure ("impossible"^(string_of_int __LINE__)))

let _lt (arg : value list) : value =
  match List.length arg with
  | 1 -> true |> bool2value
  | 2 ->
     (match arg with
      | [Vwarp (Number n1); Vwarp (Number n2)] -> n1 < n2 |> bool2value
      | _ -> raise (Error.wrong_type_arg_error "<"))
  | _ -> raise (Error.wrong_number_of_arguments "<")
		   
let _le (arg: value list) : value =
  match List.length arg with
  | 1 -> true |> bool2value
  | 2 -> 
     (try if equal arg |> value2bool then bool2value true
	  else _lt arg with
      | Error.Wrong_type_arg _ -> raise (Error.wrong_type_arg_error "<="))
  | _ -> raise (Error.wrong_number_of_arguments "<=")

let _gt (arg : value list) : value =
  match List.length arg with
  | 1 -> true |> bool2value
  | 2 -> (try _le arg |> value2bool |> not |> bool2value with
	  | Error.Wrong_type_arg _ -> raise (Error.wrong_type_arg_error ">"))
  | _ -> raise (Error.wrong_number_of_arguments ">")
let _ge (arg : value list) : value =
  match List.length arg with
  | 1 -> true |> bool2value
  | 2 -> (try (if equal arg |> value2bool then true |> bool2value
	      else _gt arg) with
	 | Error.Wrong_type_arg _ -> raise (Error.wrong_type_arg_error ">="))
  | _ -> raise (Error.wrong_number_of_arguments ">=")

let funcall (arg : value list) (env:environ) : value =
  match List.length arg with
  | 0 ->  raise (Error.wrong_number_of_arguments "funcall") 
  | _ ->
     Eval.func_call
       (try List.hd arg |> unwarp_Vfunc  with
	| Error.Wrong_type_arg _ -> raise (Error.wrong_type_arg_error "funcall"))
       (List.tl arg)
       env

  
;;
let load_builtin () =   
  let () = aux_set "defun" (Vmacro (SpecialM defun)) in
  let () = aux_set "lambda" (Vmacro (NoOutEvalM lambda)) in
  let () = aux_set "defmacro" (Vmacro (SpecialM defmacro)) in
  let () = aux_set "car" (Vfunc (BuiltInF car)) in
  let () = aux_set "cdr" (Vfunc (BuiltInF cdr)) in
  let () = aux_set "cons" (Vfunc (BuiltInF cons)) in
  let () = aux_set "+" (Vfunc (BuiltInF add)) in
  let () = aux_set "-" (Vfunc (BuiltInF sub)) in
  let () = aux_set "*" (Vfunc (BuiltInF mul)) in
  let () = aux_set "/" (Vfunc (BuiltInF div)) in
  (* let () = aux_set "'" (Vmacro (NoOutEvalM quote)) in *)
  let () = aux_set "quote" (Vmacro (NoOutEvalM quote)) in
  let () = aux_set "list" (Vfunc (BuiltInF list)) in
  let () = aux_set "quasiquote" (Vmacro (BuiltInM quasiquote)) in
  let () = aux_set "set" (Vfunc (SpecialF set)) in
  let () = aux_set "let" (Vmacro (NoOutEvalM _let)) in
  let () = aux_set "equal" (Vfunc (BuiltInF equal)) in
  let () = aux_set "if" (Vmacro (NoOutEvalM _if)) in
  let () = aux_set "<" (Vfunc (BuiltInF _lt)) in
  let () = aux_set "<=" (Vfunc (BuiltInF _le)) in
  let () = aux_set ">" (Vfunc (BuiltInF _gt)) in
  let () = aux_set ">=" (Vfunc (BuiltInF _ge)) in
  let () = aux_set "funcall" (Vfunc (SpecialF funcall)) in
  ()

  
