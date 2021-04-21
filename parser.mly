/* SOS Parser */

%{ open Ast %}

/* Declarations */

/* %token statements... */
%token ADD SUB MUL MMUL DIV MOD SEQ
%token NOT EQ LT GT LTEQ GTEQ EQEQ NEQ AND OR
%token CONCAT OF
%token DOT COMMA COLON DOLLAR
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token IF THEN ELSE
%token STRUCT ALIAS ARRAY FUNC TO
%token <Ast.program> IMPORT
%token <int> INTLIT
%token <string> FLOATLIT
%token <bool> BOOLLIT
%token <string> VAR
%token EOF

%start program
%type <Ast.program> program

/* Associativity and Precedence */
%right VAR
%nonassoc IF THEN ELSE
%left COMMA 
%right EQ
%left SEQ
%left AND OR
%left EQEQ NEQ
%left LT GT LTEQ GTEQ
%left OF
%left CONCAT
%left ADD SUB
%right MMUL
%left MUL DIV MOD
%nonassoc LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE
%right NOT
%left DOT


%%

/* rules */
typeid:
    VAR { TypeID($1) }
  | ARRAY typeid { ArrayTypeID($2) }
  | FUNC types TO typeid { FxnTypeID($2, $4) } 

value:
    VAR { Var ($1) }
  | value DOT VAR { StructField($1, $3) }
  | value LBRACK expr RBRACK { ArrayAccess($1, $3) }
  | DOLLAR LPAREN expr RPAREN { $3 }
  | fxn_app { $1 }

fxn_app:
    value LPAREN args RPAREN { FxnApp($1, $3) }

stexpr:
    VAR COLON typeid EQ expr { VarDef($3, $1, $5) }
  | VAR COLON LPAREN fxn_args RPAREN TO typeid EQ expr { FxnDef($7,$1,$4,$9) }
  | VAR EQ expr { Assign ($1, $3) }
  | value DOT VAR EQ expr { AssignStruct($1, $3, $5) }
  | value LBRACK expr RBRACK EQ expr { AssignArray($1, $3, $6) }
  | IF expr THEN expr ELSE expr { IfElse($2,$4,$6) }
  | fxn_app { $1 }

expr:
    INTLIT { IntLit($1) }
  | FLOATLIT { FloatLit($1) }
  | BOOLLIT { BoolLit($1) }
  | NOT expr { Uop(Not,$2) }
  | SUB expr { Uop(Neg,$2) }
  | expr ADD expr { Binop($1,Add,$3) }
  | expr SUB expr { Binop($1,Sub,$3) }
  | expr MUL expr { Binop($1,Mul,$3) }
  | expr MMUL expr { Binop($1,MMul,$3) }
  | expr DIV expr { Binop($1,Div,$3) }
  | expr MOD expr { Binop($1,Mod,$3) }
  | expr EQEQ expr { Binop($1,Eq,$3) }
  | expr NEQ expr { Binop($1,Neq,$3) }
  | expr LT expr { Binop($1,Less,$3) }
  | expr GT expr { Binop($1,Greater,$3) }
  | expr LTEQ expr { Binop($1,LessEq,$3) }
  | expr GTEQ expr { Binop($1,GreaterEq,$3) }
  | expr AND expr { Binop($1,And,$3) }
  | expr OR expr { Binop($1,Or,$3) }
  | expr SEQ expr { Binop($1,Seq,$3) }
  | expr CONCAT expr {Binop($1,Concat,$3) }
  | expr OF expr { Binop($1,Of,$3) }
  | LPAREN expr RPAREN { $2 }
  | VAR LBRACE args RBRACE { NamedStruct($1, $3) }
  | LBRACE args RBRACE { AnonStruct($2) }
  | LBRACK args RBRACK { ArrayCon($2) }
  | VAR { Var ($1) }
  | value DOT VAR { StructField($1, $3) }
  | value LBRACK expr RBRACK {ArrayAccess($1, $3) }
  | stexpr { $1 }

fxn_args:
    /* nothing */ { [] }
  | fxn_args_list {List.rev $1}

fxn_args_list:
    typeid VAR { [($1,$2)] }
  | fxn_args_list COMMA typeid VAR { ($3,$4) :: $1 }

args:
    /* nothing */ { [] }
  | args_list {List.rev $1}

args_list:
    expr { [$1] }
  | args_list COMMA expr { $3 :: $1 }

types:
    /* nothing */ { [] }
  | rev_types {List.rev $1}

rev_types:
    typeid { [$1] }
  | rev_types COMMA typeid { $3 :: $1 }

typedef:
    ALIAS VAR EQ typeid { Alias($2,$4) }
  | STRUCT VAR EQ LBRACE fxn_args RBRACE { StructDef($2,$5) }

stmt:
    typedef { Typedef($1) }
  | stexpr { Expression($1) }

stmts:
    stmt { [$1] }
  | stmts stmt { $2:: $1 }

program:
    stmts EOF { List.rev $1 }
  | IMPORT program { $1 @ $2 }
