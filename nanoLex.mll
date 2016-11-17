{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
    eof         { EOF }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | ['0'-'9']+  { Num (int_of_string (Lexing.lexeme lexbuf)) }
  | [' ''\t''\n''\r']      { token lexbuf }
  | "let"       { LET }
  | "rec"       { REC }
  | "="         { EQ }
  | "in"        { IN }
  | "->"        { ARROW }
  | "fun"       { FUN }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*  { Id (Lexing.lexeme lexbuf) }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { MUL }
  | "/"         { DIV }
  | "<"         { LT }
  | "<="        { LE }
  | "!="        { NE }
  | "&&"        { AND }
  | "||"        { OR }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "::"        { COLONCOLON }
  | "["         { LBRAC }
  | "]"         { RBRAC }
  | ";"         { SEMI }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
