type operator = 
(*num operators*)
Add | Sub | Mul | Div | Mod | Pow 
(*relational operators*)
| Eq | Neq | Less | Greater | LessEq | GreaterEq 
(*boolean operators*)
| And | Or 
(*sequencing*)
| Seq


type uop = Not | Neg

type id = string (* non-type id *)
type tid = (* type id *)
  TypeID of string
| ArrayTypeID of tid
type import = string

(* type name pair *)
type argtype = id * id
(* name:expr pair *)
type namedArg = id * expr

(* all possible expression statements, found in LRM sec 4 *)
and expr = 
  VarDef of tid * id * expr                (* type name = val *)
| FxnDef of tid * id * argtype list * expr (* type id (type name, ...) = val *)
| Assign of id * expr                      (* id = val *)
| AssignStruct of id * id * expr           (* id.field = val *)
| AssignArray of id * expr * expr          (* id[expr] = expr *)
| Uop of uop * expr                        (* uop expr *)
| Binop of expr * operator * expr          (* expr op expr *)
(*| OrderedFxnApp of id * expr list          (* name(expr, ...) *)
| NamedFxnApp of id * namedArg list  *)      (* name(id:expr, ...) *)
| FxnApp of id * fxnargs
| IfElse of expr * expr * expr             (* if expr then expr else expr *)
| ArrayCon of expr list                    (* [expr, ...] *)
| AnonStruct of expr list                  (* {expr, ...} *)
| NamedStruct of id * expr list           (* name{expr, ...} *)
| Var of id                                (* name *)
| StructField of id * id                   (* name.id *)
| IntLit of int                            (* int *)
| FloatLit of float                        (* float *)

and fxnargs = 
  OrderedFxnArgs of expr list
| NamedFxnArgs of namedArg list

type typedef = 
  Alias of tid * tid                       (* alias name = type *)
| StructDef of id * argtype list           (* struct name = {type name, ...} *)

type stmt = 
  Typedef of typedef
| Expression of expr
| Import of import

type program = stmt list
