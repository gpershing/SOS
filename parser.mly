/* SOS Parser */

%{ open Ast %}

/* Declarations */

/* %token statements... */
%token ADD SUB MUL MMUL DIV MOD POW SEQ
%token NOT EQ LT GT LTEQ GTEQ EQEQ NEQ AND OR
%token CONCAT OF
%token DOT COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token IF THEN ELSE
%token STRUCT ALIAS ARRAY
%token <Ast.program> IMPORT
%token <int> INTLIT
%token <string> FLOATLIT
%token <bool> BOOLLIT
%token <string> VAR
%token <string> VARCOLON
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
%right POW
%nonassoc LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE
%right NOT
%left DOT


%%

/* rules */
typeid:
    VAR { TypeID($1) }
  | ARRAY typeid { ArrayTypeID($2) }

/* There are three types of expressions:
 * A statement expression (stexpr)
   - These can only start an expression or be used in a safe context
 * A safe statement expression (safe_stexpr)
   -  A statement expression that can be used as an expression
 * A normal (non-statement) expression
   - All other expressions
 * Certain expressions can introduce protected expressions
   - Mainly through parens
   - Only place where any expression can be used */

/* While convoluted, it allows for a syntax without expression endings
   Notice that stexprs that start with VAR VAR can be ambiguous in many
   cases. */

protected_expr: /* Any chance here MUST be reproduced in stexpr */
    VAR VAR EQ expr { VarDef (TypeID($1), $2, $4) }
  | VAR VAR LPAREN fxn_args RPAREN EQ expr { FxnDef(TypeID($1),$2,$4,$7) }
  | ARRAY typeid VAR EQ expr { VarDef (ArrayTypeID($2), $3, $5) }
  | ARRAY typeid VAR LPAREN fxn_args RPAREN EQ expr 
    { FxnDef(ArrayTypeID($2), $3, $5, $8) }
  | expr { $1 }

stexpr:
    VAR VAR EQ expr { VarDef (TypeID($1), $2, $4) }
  | VAR VAR LPAREN fxn_args RPAREN EQ expr { FxnDef(TypeID($1),$2,$4,$7) }
  | ARRAY typeid VAR EQ expr { VarDef (ArrayTypeID($2), $3, $5) }
  | ARRAY typeid VAR LPAREN fxn_args RPAREN EQ expr 
    { FxnDef(ArrayTypeID($2), $3, $5, $8) }
  | safe_stexpr { $1 }

safe_stexpr:
    VAR DOT VAR EQ expr { AssignStruct(Var($1), $3, $5) }
  | VAR LBRACK protected_expr RBRACK EQ expr { AssignArray(Var($1),$3,$6) }
  | VAR EQ expr { Assign($1, $3) }
  | VAR LPAREN either_args RPAREN { FxnApp($1, $3) }
  | IF expr THEN expr ELSE expr { IfElse($2,$4,$6) }
  
expr:
    INTLIT { IntLit($1) }
  | FLOATLIT { FloatLit($1) }
  | BOOLLIT { BoolLit($1) }
  | VAR { Var($1) }
  | expr DOT VAR { StructField($1,$3) }
  | VAR DOT VAR { StructField(Var($1), $3) }
  | NOT expr { Uop(Not,$2) }
  | SUB expr { Uop(Neg,$2) }
  | expr ADD expr { Binop($1,Add,$3) }
  | expr SUB expr { Binop($1,Sub,$3) }
  | expr MUL expr { Binop($1,Mul,$3) }
  | expr MMUL expr { Binop($1,MMul,$3) }
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
  | expr CONCAT expr {Binop($1,Concat,$3) }
  | expr OF expr { Binop($1,Of,$3) }
  | expr DOT VAR EQ expr { AssignStruct($1, $3, $5) }
  | expr LBRACK protected_expr RBRACK EQ expr { AssignArray($1, $3, $6) }
  | LPAREN protected_expr RPAREN { $2 }
  | VAR LBRACE args RBRACE { NamedStruct($1, $3) }
  | VAR LBRACK protected_expr RBRACK { ArrayAccess (Var($1), $3) }
  | expr LBRACK protected_expr RBRACK { ArrayAccess($1, $3) }
  | LBRACE args RBRACE { AnonStruct($2) }
  | LBRACK args RBRACK { ArrayCon($2) }
  | safe_stexpr { $1 }
    

either_args:
  | VARCOLON protected_expr COMMA named_args { NamedFxnArgs (($1, $2) :: $4) }
  | args { OrderedFxnArgs ($1) }

fxn_args:
    /* nothing */ { [] }
  | fxn_args_list {List.rev $1}

fxn_args_list:
    typeid VAR { [($1,$2)] }
  | fxn_args_list COMMA typeid VAR { ($3,$4) :: $1 }

named_args:
    /* nothing */ { [] }
  | named_args_list {List.rev $1}

named_args_list:
    VARCOLON protected_expr { [($1,$2)] }
  | named_args_list COMMA VARCOLON expr { ($3,$4) :: $1 }

args:
    /* nothing */ { [] }
  | args_list {List.rev $1}

args_list:
    protected_expr { [$1] }
  | args_list COMMA protected_expr { $3 :: $1 }

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
