(* SOS Scanner *)

{ open Parser }

(* Definitions *)
let digit = ['0'-'9']
let digits = digit+

(* Rules *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"     { comment 0 lexbuf }           (* Comments *)
| "//"     { single_comment lexbuf }            (* Single line comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ','      { COMMA }
| '.'      { DOT }
| ':'      { COLON }
| '+'      { ADD }
| '-'      { SUB }
| '*'      { MUL }
| '/'      { DIV }
| '%'      { MOD }
| '^'      { POW }
| ';'      { SEQ }
| "=="     { EQEQ }
| "!="     { NEQ }
| '!'      { NOT }
| '='      { EQ }
| '<'      { LT }
| '>'      { GT }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "then"   { THEN }
| "else"   { ELSE }
| "struct" { STRUCT }
| "alias"  { ALIAS }
| digits as lxm { INTLIT(int_of_string lxm) } 
(*| digit+ as lxm { INTLIT(int_of_string lxm) } *)
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLOATLIT(float_of_string lxm) }
(*| digit+ '.'  digit* ( ['e' 'E'] ['+' '-']? digit )? as lxm { FLOATLIT(float_of_string lxm) }*)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { VAR(lxm) }
| eof { EOF }

and comment depth = parse
  "*/" { if depth==0 then token lexbuf else comment (depth-1) lexbuf }
| "/*" { comment (depth+1) lexbuf }
| _    { comment depth lexbuf }

and single_comment = parse
  '\n' { token lexbuf }
| _    { single_comment lexbuf }
