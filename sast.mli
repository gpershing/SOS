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
| SAssignStruct of id * id * sexpr           (* id.field = val *)
| SAssignArray of id * sexpr * sexpr          (* id[expr] = expr *)
| SUop of uop * sexpr                        (* uop expr *)
| SBinop of sexpr * operator * sexpr          (* expr op expr *)
| SFxnApp of id * sfxnargs
| SIfElse of sexpr * sexpr * sexpr             (* if expr then expr else expr *)
| SArrayCon of sexpr list                    (* [expr, ...] *)
| SAnonStruct of sexpr list                  (* {expr, ...} *)
| SNamedStruct of id * sexpr list           (* name{expr, ...} *)
| SVar of id                                (* name *)
| SStructField of id * id                   (* name.id *)
| SIntLit of int                            (* int *)
| SFloatLit of string                       (* float *)
| SBoolLit of bool                          (* bool *)

and snamedArg = id * sexpr 

and sfxnargs = 
  SOrderedFxnArgs of sexpr list
| SNamedFxnArgs of snamedArg list

type stypedef = 
  SAlias of id * typeid
| SStructDef of id * sargtype list

type sstmt = 
  STypeDef of stypedef
| SExpression of sexpr
| SImport of import

type sprogram = sstmt list
