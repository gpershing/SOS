(* Code generation: translate takes a semantically checked AST and
produces LLVM IR *)
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate stmts =
  let context = L.global_context () in

  let the_module = L.create_module context "SOS" in
  (* *)

  let build_stmt stmtdecl =
  (* *)
  
  in
  List.iter build_stmt stmts;
  the_module