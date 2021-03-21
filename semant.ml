(* Semantic checking for the SOS compiler *)

open Ast
open Sast

(* import map for global variables (VarDef, FxnDef, Alias, StructDef) *)
(* we don't have scope defined so a string * tid is enough? *)
module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each statement *)

(* Environment type for holding all the bindings currently in scope *)
type environment {
  typemap : typeid StringMap.t;
  varmap : typeid StringMap.t;
  funcmap : func_bind StringMap.t;
}


let check prog =

  (* add built-in function such as basic printing *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      ftype = Void;
      formals = [(ty, "x")] (* maybe need locals here? *)} map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("printb", Bool);
			                         ("printf", Float)]
  in

  (* add built-in types such as int, float *)
  let built_in_types = 
    let add_type map (name, ty) = StringMap.add name ty map
    in List.fold_left add_type StringMap.empty [("int", Int), ("bool", Bool), ("float", Float), ("void", Void)]
  in

  (* Initial environment containing built-in types and functions *)
  let global_env = { typemap: build_in_types; varmap: StringMap.empty; funcmap: build_in_decls }
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

  (* should add a function to add three things above dynamically *)
  let add_id_type = ()
  in

  (* function to lookup *)
  let type_of_id s = ()
  in

  (* maybe it is better do define check funcs for each
     alias, struct and expr here *)
  let rec expr env = function
      IntLit i -> ((TypeID("int"), SIntLit i), env)
    | FloatLit f -> ((TypeID("float"), SFloatLit i), env)
    | FxnApp(id, fxnargs) -> () (* TODO: need to check if it is match *)
    | _ -> ()

  (* return checked sast, a long function *)
  let rec stmt env = function
      Expression(e) -> let (se, en) = expr env e in (SExpression (se), en)
    | _ -> ()

  let rec stmts env = function 
    hd :: tl -> let (st, en) = stmt hd in st :: stmts en tl
    _ -> []
  in stmts prog global_env
