(* open Util *)
       
type sexp =
  | Number of int
  | String of string
  | Bool of bool
  | Symbol of string
  | Slist of sexp list
  | DotSlist of sexp list * sexp
			 
 and value =
   | Vfunc   of func
   | Vmacro  of macro
   | Vwarp   of sexp

 and func =
   | BuiltInF of (value list -> value)
   | UserDefF of (value list -> value)
   | SpecialF of (value list -> environ -> value)
		   
 and macro =
   | BuiltInM of (sexp list -> sexp)
   | UserDefM of (sexp list -> sexp)
   | SpecialM of (sexp list -> environ -> sexp)
   | NoOutEvalM of (sexp list -> environ -> value)
		   
 and environ =
   {
     mutable varmap : value Map.Make(String).t;
     mutable fmmap  : value Map.Make(String).t;
     upper_env : environ option
   }

let nil = Vwarp (Slist [])
