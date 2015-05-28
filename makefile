CC = ocamlc
CC1 = ocamlc
CC2 = ocamlopt
SET1 = parser.cmo lexer.cmo type.cmo util.cmo error.cmo env.cmo eval.cmo stdlib.cmo builtin.cmo  init.cmo
SET2 = parser.cmx lexer.cmx type.cmx util.cmx error.cmx env.cmx eval.cmx stdlib.cmx builtin.cmx  init.cmx

all : lexer parser type util error eval env builtin init
	$(CC1) -o lisp $(SET1)
	@echo "done"

native : lexer parser type util error eval env builtin init 
	$(CC2) -o lisp $(SET2)
	@echo "done"

eval : eval.ml type env
	$(CC) -c eval.ml
env : env.ml util error
	$(CC) -c env.ml

parser:parser.ml parser.mli type
	$(CC) -c parser.mli
	$(CC) -c parser.ml

lexer: lexer.ml parser
	$(CC) -c lexer.ml

type: type.ml 
	$(CC) -c type.ml
util: util.ml type 
	$(CC) -c util.ml
error: error.ml util
	$(CC) -c error.ml
builtin : builtin.ml type util env 
	$(CC) -c builtin.ml
init : init.ml type util builtin error stdlib
	$(CC) -c init.ml
stdlib: stdlib.ml eval parser lexer
	$(CC) -c stdlib.ml
# ---------------------------------------

lexer.ml: lexer.mll 
	ocamllex lexer.mll
parser.ml: parser.mly
	ocamlyacc parser.mly
parser.mli: parser.mly
	ocamlyacc parser.mly



clean :
	rm *.cm* *.o
	rm lexer.ml parser.ml parser.mli lisp
