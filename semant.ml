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
    let add_bind map (name, ty) = StringMap.add name {
      typ = TypeID("void");
      fname = name;
      formals = [(ty, "x")] (* maybe need locals here? *)} map
    in List.fold_left add_bind StringMap.empty [ ("print", TypeID("int"));
			                         ("printb", TypeID("bool"));
			                         ("printf", TypeID("float"))]
  in

  (* add built-in types such as int, float *)
  let built_in_types = ()
  in

  (* maybe we could also add std graph lib here? *)
  let stdlib = ()
  in

  (* check a list of function formals & locals has no undecleared type and no duplicates *)
  (* Un decleared types are those not in stdlib and undecleared alias & struct *)
  let check_fxn_argtype (kind : string) (binds : argtype list) = ()
  in

  (* check if rvalue type could be assigned to lvalue type *)
  let check_assign lvaluet rvaluet err = ()
  in

  (* expr type table: shown globals so far, formals and shown locals for each function scope *)
  let symbols = ()
  in

  (* should add a function to add three things above dynamically *)
  let add_id_type = ()
  in

  (* function to lookup *)
  let type_of_id s = ()
  in

  (* maybe it is better do define check funcs for each
     alias, struct and expr here *)
  let rec expr = function
      IntLit i -> (TypeID("int"), SIntLit i)
    | FloatLit f -> (TypeID("float"), SFloatLit i)
    | FxnApp(id, fxnargs) -> () (* TODO: need to check if it is match *)
    | _ -> ()

  (* return checked sast, a long function *)
  let rec stmt = function
      Expression(e) -> SExpression (expr e)
    | _ -> ()

in stmts