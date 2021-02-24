/* SOS Parser */

%{ open Ast %}

/* Declarations */

/* %token statements... */
%token ADD SUB MUL DIV MOD POW SEQ
%token NOT EQ LT GT LTEQ GTEQ EQEQ NEQ AND OR
%token DOT COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token IF THEN ELSE
%token STRUCT ALIAS
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> VAR
%token <string> VARCOLON
%token EOF

%start program
%type <Ast.program> program

/* Associativity and Precedence */
%left SEQ
%nonassoc IF THEN ELSE
%left COMMA 
%right EQ
%left AND OR
%left EQEQ NEQ
%left LT GT LTEQ GTEQ
%left ADD SUB
%left MUL DIV MOD
%right POW
%nonassoc LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE
%right NOT
%left DOT VAR


%%

/* rules */


stexpr:
    VAR DOT VAR EQ expr { AssignStruct($1, $3, $5) }
  | VAR LBRACK expr RBRACK EQ expr { AssignArray($1, $3, $6) }
  | VAR EQ expr { Assign($1, $3) }
  | VAR VAR EQ expr { VarDef($1,$2,$4) }
  | VAR VAR LPAREN fxn_args RPAREN EQ expr { FxnDef($1,$2,$4,$7) }
  | VAR LPAREN either_args RPAREN { FxnApp($1, $3) }
  | IF expr THEN expr ELSE expr { IfElse($2,$4,$6) }
  
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
  | expr LTEQ expr { Binop($1,LessEq,$3) }
  | expr GTEQ expr { Binop($1,GreaterEq,$3) }
  | expr AND expr { Binop($1,And,$3) }
  | expr OR expr { Binop($1,Or,$3) }
  | expr SEQ expr { Binop($1,Seq,$3) }
  | LPAREN expr RPAREN { $2 }
  | VAR LBRACE args RBRACE { NamedStruct($1, $3) }
  | LBRACE args RBRACE { AnonStruct($2) }
  | LBRACK args RBRACK { ArrayCon($2) }
  | stexpr { $1 }

either_args:
  | VARCOLON expr COMMA named_args { NamedFxnArgs (($1, $2) :: $4) }
  | args { OrderedFxnArgs ($1) }

fxn_args:
    /* nothing */ { [] }
  | fxn_args_list {List.rev $1}

fxn_args_list:
    VAR VAR { [($1,$2)] }
  | fxn_args_list COMMA VAR VAR { ($3,$4) :: $1 }

named_args:
    /* nothing */ { [] }
  | named_args_list {List.rev $1}

named_args_list:
    VARCOLON expr { [($1,$2)] }
  | named_args_list COMMA VARCOLON expr { ($3,$4) :: $1 }

args:
    /* nothing */ { [] }
  | args_list {List.rev $1}

args_list:
    expr { [$1] }
  | args_list COMMA expr { $3 :: $1 }

typedef:
    ALIAS VAR EQ VAR { Alias($2,$4) }
  | STRUCT VAR EQ LBRACE fxn_args_list RBRACE { StructDef($2,$5) }

stmt:
    typedef { Typedef($1) }
  | stexpr { Expression($1) }

stmts:
    stmt { [$1] }
  | stmts stmt { $2:: $1 }

program:
    stmts EOF { List.rev $1 }
