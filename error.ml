(* handle errors  *)

exception ShouldNotHappened of string

let unbound_var name =
  (Failure (Printf.sprintf "unbound variable : %s" name))

let unbound_func name =
  (Failure (Printf.sprintf "void-function : %s" name))

(* let unpack_error () = *)
(*   raise (ShouldNotHappened "") *)

exception No_dotlist_eval of string
let no_dotlist_eval exp =
  let buf = Buffer.create 1 in
  Util.print_sexp buf exp;
  (No_dotlist_eval (Buffer.to_bytes buf))

(* let not_implement_yet (str: string) = (\* 写在还没实现的地方 *\) *)
(*   print_bytes str *)
let wrong_number_of_arguments name =
  Failure (Printf.sprintf "wrong number of arguments : %s" name)
	  
exception Wrong_type_arg of string
	    
let wrong_type_arg_error str=
  Wrong_type_arg str
	  
exception Unwarp_value
let unwarp_value_error =
  Unwarp_value

exception Div_zero
let div_zero_error =
  Div_zero
