%{
   open Type
%}

%token LPAREN RPAREN QUOTE DOT QUASIQUOTE UNQUOTE UNQUOTE_SPLICING EOF
%token <int> NUMBER
%token <string> SYMBOL
%token <string> STRING
%token <bool> BOOL
%start parse
%type<Type.sexp list option> parse
%%

parse :
    | EOF {None}
    | exps {Some $1}

exp:
    | NUMBER {Number $1}
    | STRING {String $1}
    | BOOL   {Bool   $1}
    | SYMBOL {Symbol $1}
    | LPAREN exps RPAREN  {Slist $2}
    | LPAREN RPAREN       {Slist []}
    | LPAREN exps DOT exp RPAREN  {DotSlist ($2, $4)}
    | LPAREN DOT exp RPAREN       {DotSlist ([], $3)} 
    | QUOTE exp  {Slist [Symbol "quote"; $2]}
    | QUASIQUOTE exp  {Slist [Symbol "quasiquote"; $2]}
    | UNQUOTE exp   {Slist [Symbol "unquote"; $2]}
    | UNQUOTE_SPLICING exp {Slist [Symbol "unquote_splicing"; $2]}

exps:
    | exp exps {$1 :: $2}
    | exp      {[$1]}
