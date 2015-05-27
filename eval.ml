open Type
       
let rec eval (exp:sexp) (env:environ) : value =
  match exp with
  | Number _     
  | String _
  | Bool   _  as s -> Vwarp s
  | Symbol name -> Env.get_var name env
  | Slist  sexp -> 
     begin
       match sexp with
       | [] -> Vwarp (Slist [])
       | hd :: tl ->
	  let hd' = match hd with
	    | Symbol name -> Env.get_fm name env
	    | s -> eval hd env
	  in
	  match hd' with
	  | Vfunc func ->
	     func_call func (List.map (fun a -> eval a env) tl) env
	  | Vmacro macro ->  macro_call macro tl env
	  | Vwarp s -> raise (Error.unbound_func (Util.sprint_sexp s))
     end
  |DotSlist (sexp_lst, sexp) -> raise (Error.no_dotlist_eval sexp)


and func_call (func : func) (args : value list) (env:environ): value =
  (* let args' = List.map *)
  (* 		(fun a -> match a with *)
  (* 			  | Vwarp s -> eval s env *)
  (* 			  | _ -> a *)
  (* 		) *)
  (* 		args *)
  let args' = args in
  match func with
  | BuiltInF f -> f args'
  | UserDefF f -> f args'
  | SpecialF f -> f args' env

and macro_call (macro : macro) (args : sexp list) (env : environ) : value =
  match macro with
  | NoOutEvalM m -> m args env
  | SpecialM   m -> eval (m args env) env
  | UserDefM   m -> eval (m args) env
  | BuiltInM   m -> eval (m args) env



let rec eval_sexp_list (exps: sexp list) (env: environ) : value =
  let rst = ref nil in
  begin
    List.iter (fun s -> rst := eval s env) exps;
    !rst
  end

let define_func  ?(tp="userdef") (func_name:string) (args:string list) (body: sexp list) (env:environ) : func =
  let env' = Env.new_env env in
  let aux_func args' = 
    if (List.length args') <> (List.length args)
    then raise (Error.wrong_number_of_arguments func_name)
    else
      let aux_set_var k v = Env.set_var k v env' in
      let () = List.iter2 aux_set_var args args' in
      let rst = ref nil in
      body |> List.iter (fun s -> rst := (eval s env')) ;
      !rst
  in
  if tp = "userdef" then
    UserDefF aux_func 
  else BuiltInF aux_func 
      
(* define macro doesn't need env also when calling it  *)
(* 上面这句不对－ －。。。，macro 展开显然需要 env 的， 而且是当前的 env， 类似动态作用域 *)
(* 仔细试了 几次 elisp的macro： *)
(* macro 的 body 先被执行一遍（当然是在参数替换完之后）， 把执行结果作为macro的返回值，然后这个返回值再被执行一遍！*)
(* 假设 lisp 方言的macro的执行大致都是按照这个步骤的。 *)
let define_macro ?(tp="userdef") (macro_name:string) (args:string list) (body:sexp list) (env:environ) : macro =
  let env' = Env.new_env env in
  let _macro args' =
    if (List.length args') <> (List.length args)
    then raise (Error.wrong_number_of_arguments macro_name)
    else
      let aux_set_var k v = Env.set_var k v env' in
      let () = List.iter2 aux_set_var args (List.map (fun a -> Vwarp a) args') in
      match eval_sexp_list body env' with
      | Vwarp s -> s
      | _ -> raise Error.unwarp_value_error
  in
  if tp = "userdef" then UserDefM _macro
  else BuiltInM _macro
      
      
      
(* let rec scan_and_eval_unquote (sexp:sexp) (env:environ) : sexp = *)
(*   let aux s : sexp=  *)
(*     match s with *)
(*     | Number _ *)
(*     | String _ *)
(*     | Bool _ *)
(*     | Symbol _ *)
(*     | Slist [] as x -> x *)
(*     | Slist [(Symbol "unquote");tl] -> *)
(*        begin *)
(* 	 print_bytes "shit"; *)
(* 	 match scan_and_eval_unquote tl env with *)
(* 	 | (s:sexp) -> s *)
(*        end *)
(*     | Slist _ as sl -> sl *)
(*     | DotSlist _ as ds -> ds *)
(*   in *)
(*   match sexp with *)
(*   | Slist [] as x -> x *)
(*   | Slist l -> 	 Slist (List.map aux l) *)
(*   | _ -> raise (Failure ("impossible" ^ (string_of_int __LINE__))) *)

(* let rec scan_and_eval_unquote2 (sexp:sexp) (env:environ) :sexp = *)
(*   match  *)
