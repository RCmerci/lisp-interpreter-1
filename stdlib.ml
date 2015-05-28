let stdlib =
"
(defun not (x)
  (if x #f #t))
(defun null? (x)
  (if (equal '() x)
      #t
    #f))
(defun abs (x)
  (if (< x 0)
      (- x)
    x))
(defun foldl (f init l)
  (if (equal l '())
      init
    (foldl f (funcall f init (car l)) (cdr l))))
"


let load_stdlib env  =
  let eval sexpl = Eval.eval_sexp_list sexpl env in
  (match Parser.parse Lexer.tokens (Lexing.from_string stdlib) with
   | Some sexpl -> sexpl
   | None -> [])
  |> eval

