open Type

let top_env = {
    varmap=Util.StrMap.empty;
    fmmap =Util.StrMap.empty;
    upper_env=None;
  };;

let aux_set (name:string) (value:value) = Env.set_fm name value top_env


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
	  
;;
let load_builtin () =   
  let () = aux_set "defun" (Vmacro (SpecialM defun)) in
  let () = aux_set "defmacro" (Vmacro (SpecialM defmacro)) in
  let () = aux_set "car" (Vfunc (BuiltInF car)) in
  let () = aux_set "cdr" (Vfunc (BuiltInF cdr)) in
  let () = aux_set "+" (Vfunc (BuiltInF add)) in
  let () = aux_set "-" (Vfunc (BuiltInF sub)) in
  let () = aux_set "*" (Vfunc (BuiltInF mul)) in
  let () = aux_set "/" (Vfunc (BuiltInF div)) in
  (* let () = aux_set "'" (Vmacro (NoOutEvalM quote)) in *)
  let () = aux_set "quote" (Vmacro (NoOutEvalM quote)) in
  let () = aux_set "list" (Vfunc (BuiltInF list)) in
  let () = aux_set "quasiquote" (Vmacro (BuiltInM quasiquote)) in
  let () = aux_set "set" (Vfunc (SpecialF set)) in
  ()

  
