open Type
(* util functions and modules here *)

module StrMap = Map.Make(String)

let unpack_slist (exp_lst: sexp) : sexp =
  (* unpack `Slist of sexp list`   *)
  (* it should be just one element in `sexp list` *)
  match exp_lst with
  | Slist [e] -> e
  | _ -> raise (Failure "unpack_error: it should not happened.")


let rec print_sexp (to_buffer: Buffer.t) (sexp: sexp) =
  let open Printf in
  match sexp with
  | Number v -> bprintf to_buffer  " %d " v
  | String v -> bprintf to_buffer " \"%s\" " v
  | Bool   v -> string_of_bool v |> bprintf to_buffer " %s "
  | Symbol v -> bprintf to_buffer " %s " v
  | Slist v_l ->
     begin
       bprintf to_buffer "(";
       List.iter (print_sexp to_buffer) v_l;
       bprintf to_buffer ")";
     end
  | DotSlist (v_l, v) ->
     begin
       bprintf to_buffer "(";
       List.iter (print_sexp to_buffer) v_l;
       bprintf to_buffer " . ";
       print_sexp to_buffer v;
       bprintf to_buffer ")";
     end

let sprint_sexp s =
  let b = Buffer.create 1 in
  print_sexp b s;
  Buffer.to_bytes b

let sprint_value v =
  (match v with
   | Vwarp s -> sprint_sexp s
   | _ -> "-") ^ "\n"
  
let sym2str (arg: sexp) : string =
  match arg with
  | Symbol name -> Printf.sprintf "%s" name
  | _ -> raise (Failure (string_of_int __LINE__))

let rec slist2string_list (s:sexp) : string list =
  match s with
  | Slist [] -> []
  | Slist l  -> List.map (fun a -> sym2str a) l
  | _ -> raise (Failure (string_of_int __LINE__))
