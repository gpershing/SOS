(* Semantic checking for the SOS compiler *)

open Ast
open Sast

(* import map for global variables (VarDef, FxnDef, Alias, StructDef) *)
(* we don't have scope defined so a string * tid is enough? *)
module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each statement *)

let check stmts =

  (* add built-in function such as basic printing *)

  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name ty map
    in List.fold_left add_bind StringMap.empty [ ("print", TypeID("int"));
			                         ("printb", TypeID("bool"));
			                         ("printf", TypeID("float"))]