%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token EOF
%token TRUE FALSE
%token <string> Id 
%token LET
%token REC
%token EQ
%token IN
%token FUN
%token ARROW
%token IF THEN ELSE
%token PLUS MINUS
%token MUL DIV
%token LT LE NE
%token AND OR
%token LPAREN RPAREN
%token LBRAC RBRAC
%token SEMI
%token COLONCOLON

%start exp 
%type <Nano.expr> exp

%%
exp: LET Id EQ exp IN exp       { Let ($2, $4, $6) }
   | LET REC Id EQ exp IN exp   { Letrec ($3, $5, $7) }
   | FUN Id ARROW exp            { Fun ($2, $4) }
   | IF exp THEN exp ELSE exp  { If ($2, $4, $6) }
   | exp1                         { $1 }

exp1: exp1 OR exp2                { Bin ($1, Or, $3) }
    | exp2                        { $1 }

exp2: exp2 AND exp3               { Bin ($1, And, $3) }
    | exp3                        { $1 }

exp3: exp3 LT exp4                { Bin ($1, Lt, $3) }
    | exp3 LE exp4                { Bin ($1, Le, $3) }
    | exp3 NE exp4                { Bin ($1, Ne, $3) }
    | exp3 EQ exp4                { Bin ($1, Eq, $3) }
    | exp4                        { $1 }

exp4: exp4 COLONCOLON exp4        { Bin ($1, Cons, $3) }
    | LBRAC exp4 exp4             { Bin ($2, Cons, $3) }
    | LBRAC exp4 RBRAC            { Bin ($2, Cons, NilExpr) }
    | SEMI exp4 exp4              { Bin ($2, Cons, $3) }
    | SEMI exp4 RBRAC             { Bin ($2, Cons, NilExpr) }
    | exp5                        { $1 }
    
exp5: exp5 PLUS exp6              { Bin ($1, Plus, $3) }
    | exp5 MINUS exp6             { Bin ($1, Minus, $3) }
    | exp6                        { $1 }

exp6: exp6 MUL exp7               { Bin ($1, Mul, $3) }
    | exp6 DIV exp7               { Bin ($1, Div, $3) }
    | exp7                        { $1 }

exp7: exp7 exp8                   { App ($1, $2)}
    | exp8                        { $1 }

exp8: LPAREN exp RPAREN          { ($2) }
    | Num                        { Const $1 }
    | TRUE                       { True }
    | FALSE                      { False }
    | Id                         { Var $1 }
    | LBRAC RBRAC                { NilExpr }
