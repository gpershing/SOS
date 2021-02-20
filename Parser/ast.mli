type operator = Add | Sub | Mul | Div | Mod | Pow | Eq | Neq | Less | Greater | And | Or | Seq

type uop = Not | Neg

type id = string

(* type name pair *)
type argtype = id * id
(* name=expr pair *)
type namedArg = id * id

(* all possible expression statements, found in LRM sec 4 *)
type expr = 
  VarDef of id * id * expr                  (* type id = val *)
| FxnDef of id * id * argtype list * expr   (* type id (type name, ...) = val *)
| Assign of id * expr                       (* id = val *)
| Uop of uop * expr                         (* uop expr *)
| Binop of expr * operator * expr           (* expr op expr *)
| OrderedFxnApp of id * expr list           (* name(expr, ...) *)
| NamedFxnApp of id * namedArg list         (* name(id=expr, ...) *)
| IfElse of expr * expr * expr              (* if expr then expr else expr *)
| ArrayCon of expr list                     (* [expr, ...] *)
| AnonStruct of expr list                   (* {expr, ...} *)
| NamedStruct of id * expr list             (* name{expr, ...} *)
| Var of id                                 (* name *)
| StructField of id * id                    (* name.id *)
| IntLit of int                             (* int *)
| FloatLit of float                         (* float *)
| Parens of expr                            (* (expr) *)

type typedef = 
  Alias of id * id                          (* alias name = type *)
| StructDef of id * argtypes                (* struct name = {type name, ...} *)

type stmt = 
  Typedef of typedef
| Expression of expr

type program = stmt list
