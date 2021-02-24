type token =
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | POW
  | SEQ
  | NOT
  | EQ
  | LT
  | GT
  | LTEQ
  | GTEQ
  | EQEQ
  | NEQ
  | AND
  | OR
  | DOT
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | IF
  | THEN
  | ELSE
  | STRUCT
  | ALIAS
  | INTLIT of (int)
  | FLOATLIT of (float)
  | VAR of (string)
  | VARCOLON of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
