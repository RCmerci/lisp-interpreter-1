open Type
open Util

let rec find ?(var=true) name env =
  (* inner use *)
  let map = if var = true then env.varmap else env.fmmap in
  try StrMap.find name map with
    Not_found -> if env.upper_env = None then raise Not_found
		 else
		   match env.upper_env with
		   | Some upper_env -> find ~var name upper_env
		   | _ -> raise (Failure "impossible")

let bind ?(var=true) name value env : unit =
  (* inner use *)
  if var = true then
    env.varmap <- StrMap.add name value env.varmap
  else
    env.fmmap <- StrMap.add name value env.fmmap
  
let get_var (name : string) (env : environ): value =
  (* 关于返回值，当值是变量的时候，没有对应env，当值是 函数 的时候， 有对应的 *)
  (* 词法环境（env） *)
  (* 修正上面的话： 词法环境在函数呗定义的时候被传进去了， 后面就不用管这个了， *)
  (* 所以，返回类型是 ｀value｀ 不是 ｀value ＊ environ option｀ *)
  try find name env with
    Not_found -> raise (Error.unbound_var name)

let set_var (name : string) (value : value) ~(env : environ) : unit =
  bind name value env

let get_fm (name : string) (env : environ) : value =
  try find ~var:false name env with
    Not_found -> raise (Error.unbound_func name)

let set_fm (name : string) (value : value) ~(env : environ) : unit =
  bind ~var:false name value env

(* let set_sexp (name: string) (sexp: sexp) (env : environ) : unit = *)
(*   bind  *)

       
(* let set_func (name : string) (value : value * environ option) (env : environ) : unit = *)
(*   bind name value env *)
       
let new_env (upper_env : environ) : environ =
  {varmap=StrMap.empty; fmmap=StrMap.empty; upper_env=Some upper_env}


    
let wrap_sexp (s:sexp) : value = Vwarp s
let unwarp_value = function
    Vwarp s -> s
  | _ -> raise (Error.unwarp_value_error)
       
  
