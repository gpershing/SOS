/* SOS Parser */

%{ open Ast %}

/* Declarations */

/* %token statements... */
%token ADD SUB MUL DIV MOD POW SEQ
%token NOT EQ LT GT AND OR EQEQ NEQ
%token DOT COMMA COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token IF THEN ELSE
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> VAR
%token EOF

%start program
%type <Ast.program> program

/* Associativity and Precedence */
%left SEQ
%left COMMA COLON
%right EQ
%left AND OR
%left EQEQ NEQ
%left LT GT
%left ADD SUB
%left MUL DIV MOD
%right POW
%right NOT
%left DOT

/* %left, %right, and %nonassoc statements */

%%

/* rules */

expr:
    INTLIT { IntLit($1) }
  | FLOATLIT { FloatLit($1) }
  | VAR { Var($1) }
  | VAR DOT VAR { StructField($1,$3) }
  | NOT expr { Uop(Not,$2) }
  | SUB expr { Uop(Neg,$2) }
  | expr ADD expr { Binop($1,Add,$3) }
  | expr SUB expr { Binop($1,Sub,$3) }
  | expr MUL expr { Binop($1,Mul,$3) }
  | expr DIV expr { Binop($1,Div,$3) }
  | expr MOD expr { Binop($1,Mod,$3) }
  | expr POW expr { Binop($1,Pow,$3) }
  | expr EQEQ expr { Binop($1,Eq,$3) }
  | expr NEQ expr { Binop($1,Neq,$3) }
  | expr LT expr { Binop($1,Less,$3) }
  | expr GT expr { Binop($1,Greater,$3) }
  | expr LT EQ expr { Binop($1,LessEq,$4) }
  | expr GT EQ expr { Binop($1,GreaterEq,$4) }
  | expr AND expr { Binop($1,And,$3) }
  | expr OR expr { Binop($1,Or,$3) }
  | expr SEQ expr { Binop($1,Seq,$3) }
  | LPAREN expr RPAREN { $2 }
  | VAR LBRACE args_list RBRACE { NamedStruct($1, $3) }
  | LBRACE args_list RBRACE { AnonStruct($2) }
  | LBRACK args RBRACK { ArrayCon($2) }
  | VAR LPAREN named_args RPAREN { NamedFxnApp($1,$3) }
  | VAR LPAREN args RPAREN { OrderedFxnApp($1,$3) }
  | IF expr THEN expr ELSE expr { IfElse($2,$4,$6) }
  | VAR VAR EQ expr { VarDef($1,$2,$4) }
  | VAR VAR LPAREN fxn_args RPAREN EQ expr { FxnDef($1,$2,$4,$7) }

fxn_args:
    /* nothing */ { [] }
  | fxn_args_list {List.rev $1}

fxn_args_list:
    VAR VAR { [($1,$3)] }
  | fxn_args_list COMMA VAR VAR { ($3,$4) :: $1 }

named_args:
    /* nothing */ { [] }
  | named_args_list {List.rev $1}

named_args_list:
    VAR COLON expr { [($1,$3)] }
  | named_args_list COMMA VAR COLON expr { ($3,$4) :: $1 }

args:
    /* nothing */ { [] }
  | args_list {List.rev $1}

args_list:
    expr { [$1] }
  | args_list COMMA expr { $3 :: $1 }
