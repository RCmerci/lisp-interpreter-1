


all : lexer parser type util error eval env builtin init
	@echo "done"


eval : eval.ml type env
	ocamlc -c eval.ml
env : env.ml util
	ocamlc -c env.ml

parser:parser.ml parser.mli type
	ocamlc -c parser.mli
	ocamlc -c parser.ml

lexer: lexer.ml parser
	ocamlc -c lexer.ml

type: type.ml 
	ocamlc -c type.ml
util: util.ml type 
	ocamlc -c util.ml
error: error.ml util
	ocamlc -c error.ml
builtin : builtin.ml type util env 
	ocamlc -c builtin.ml
init : init.ml type util builtin error
	ocamlc -c init.ml
# ---------------------------------------

lexer.ml: lexer.mll 
	ocamllex lexer.mll
parser.ml: parser.mly
	ocamlyacc parser.mly
parser.mli: parser.mly
	ocamlyacc parser.mly



clean :
	rm *.cm*
	rm lexer.ml parser.ml parser.mli
