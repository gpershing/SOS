(* Semantic checking for the SOS compiler *)

open Ast
open Sast

(* A map for global variables (VarDef, FxnDef, Alias, StructDef) - we don't have scope defined so a string type pair is enough? *)
module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each statement *)

let check stmt =

  (* add built-in function such as basic printing *)

  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name ty map
    in List.fold_left add_bind StringMap.empty [ ("print", "int");
			                         ("printb", "bool");
			                         ("printf", "float")]