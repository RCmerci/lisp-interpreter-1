open Type
open Printf
open Scanf
open Error
    
let main () =
  let () = Builtin.load_builtin () in
  (* let () =Stdlib.load_stdlib () in *)
  (* let in_chan = Scanning.stdin in *)
  let aux_f sexp =
    print_bytes (sexp^"\n");
    (try Parser.parse Lexer.tokens (Lexing.from_string sexp) with
	 | Parsing.Parse_error -> print_bytes "parse_error.";None)
    |> (function
	 | Some []  -> printf "nil \n>"
	 | Some sl -> 
	    begin
	      try Eval.eval_sexp_list sl Builtin.top_env |> Util.sprint_value |> print_bytes; printf ">" with
		      | Failure s -> print_bytes ("error:" ^ s ^ "\n>")
		      | ShouldNotHappened s -> print_bytes ("shouldnothappened:" ^ s ^ "\n>")
		      | Wrong_type_arg s -> print_bytes ("wrong_type_arg:" ^ s ^ "\n>")
		      | Unwarp_value -> print_bytes ("unwarp_value_error.\n>")
		      | Div_zero     -> print_bytes ("div_zero_error.\n>")
	    end
	 | None     -> printf "\n>"
       )
  in
  let rec loop () =
    flush stdout;
    let rec get_lines ?(rst="") inchan =
      match input_line inchan with
      | s when String.get s (pred(String.length s)) = '\\' ->
	 get_lines inchan ~rst:(rst^(String.sub s 0 ((String.length s) - 1)) ^ " ")
      | s -> rst ^ s
    in
    get_lines stdin |> aux_f;
    loop ()
  in
  print_bytes ">";
  loop ()
  
