(* Semantically-checked AST *)

open Ast

(* Detailed type meaning *)
type typeid =
  Int
| Float
| Bool
| Void
| Array of typeid
| Struct of sargtype list
| Func of typeid list * typeid
| EmptyArray (* The empty array constructor, [] *)
and sargtype = typeid * id

(* Detailed function binding *)
type func_bind = {
  ftype : typeid;
  formals : sargtype list;
}

type sexpr = typeid * sx
and sx = 
  SVarDef of typeid * id * sexpr                (* type name = val *)
| SFxnDef of typeid * id * sargtype list * sexpr (* type id (type name, ...) = val *)
| SAssign of id * sexpr                      (* id = val *)
| SAssignStruct of sexpr * id * sexpr           (* id.field = val *)
| SAssignArray of sexpr * sexpr * sexpr          (* id[expr] = expr *)
| SUop of uop * sexpr                        (* uop expr *)
| SBinop of sexpr * operator * sexpr          (* expr op expr *)
| SFxnApp of sexpr * sexpr list
| SIfElse of sexpr * sexpr * sexpr             (* if expr then expr else expr *)
| SArrayCon of sexpr list                    (* [expr, ...] *)
(* | SAnonStruct of sexpr list                  (* {expr, ...} *)
| SNamedStruct of id * sexpr list           (* name{expr, ...} *) *)
| SStruct of id * sexpr list
| SVar of id                                (* name *)
| SArrayAccess of sexpr * sexpr                (* name[expr] *)
| SArrayLength of sexpr                        (* name.length *)
| SStructField of sexpr * id                   (* name.id *)
| SIntLit of int                            (* int *)
| SFloatLit of string                       (* float *)
| SBoolLit of bool                          (* bool *)
| SCast of sexpr                            (* type casting *)

type stypedef = 
  SAlias of id * typeid
| SStructDef of id * sargtype list

type sstmt = 
  STypeDef of stypedef
| SExpression of sexpr
| SImport of import

type sprogram = sstmt list
