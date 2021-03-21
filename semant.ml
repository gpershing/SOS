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
type environment = {
  typemap : typeid StringMap.t;
  varmap : typeid StringMap.t;
  funcmap : func_bind StringMap.t;
}

type typecheck =
  TMatch
| TCast
| TNoMatch


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

  (* resolve the type of a tid to a typeid *)
  let rec resolve_typeid t map = match t with
    TypeID(s) -> if StringMap.mem s map
      then StringMap.find s map
      else raise ("Could not resolve type id "^s)
  | ArrayTypeID(s) -> Array(resolve_typeid s)
  in

  (* checks whether the given types match and, if not, whether b can be cast to a *)
  let match_type a b = 
    if a = b then TMatch else TNoMatch (* TODO Casting not currently supported *)
  in

  (* should add a function to add three things above dynamically *)
  let add_id_type = ()
  in

  (* function to lookup *)
  let type_of_id s map = 
    if StringMap.mem s map then StringMap.find s map
    else raise ("Unknown variable name "^s)
  in

  (* function to lookup the type of a struct field *)
  let type_of_field s f map = 
    let stype = type_of_id s map in
    match stype with
      Struct(sargs) -> 
      let rec find_field f = function
        (ft, fn) :: tl -> if fn = f then ft else find_field f tl
        | _ -> raise ("Could not find field "^f)
      in find_field f sargs
      _ -> raise ("Cannot access fields for a non-struct variable")

  let add_typedef td map =
    match td with
      Alias(nm, t) -> if StringMap.mem nm map
        then raise ("Cannot create an alias with preexisting name " ^ nm)
        else StringMap.add nm (resolve_typeid t) map
    | StructDef(nm, l) -> StringMap.add nm Struct(List.map (fun (t, i) -> (resolve_typeid t, i)) l)
  in

  let rec add_formals args map = match args with
    (typ, nm) :: tl -> add_formals tl StringMap.update nm (resolve_typeid typ) map
    _ -> map
  in

  (* maybe it is better do define check funcs for each
     alias, struct and expr here *)
  let rec expr env = function
      VarDef (tstr, name, exp) -> 
        let ((exptype, sexp), _) = expr env exp in
        let t = resolve_typeid tstr in
        match match_type t exptype with
          TMatch -> ((t, SVarDef(tstr, name, sexp)), { env with varmap = StringMap.update name t env.varmap } ) (* TODO may want to deal with overriding variables differently *)
          _ -> raise ("Could not match type when defining variable " ^ name)
    | FxnDef (tstr, name, args, exp) ->
        let t = resolve_typeid tstr in
        let sargs = List.map (fun (typ, nm) -> (resolve_typeid typ, nm)) in
        let newfxnmap = StringMap.update name { ftype = t; formals = sargs} env.fxnmap in
        let ((exptype, sexp), _) = expr { env with
           fxnmap = newfxnmap; 
           varmap = add_formals args varmap; }
        in
        match match_type t exptype with
          TMatch -> ((t, SFxnDef(tstr, name, sargs, sexp)), { env with fxnmap = newfxnma })
          _ -> raise ("Incorrect return type for function " ^ name)
    | Assign (name, exp) ->
         let ((exptype, sexp), _) = expr env exp in
         let t = type_of_id name in
         match match_type t exptype with
           TMatch -> ((t, SAssign(name, sexp)), env)
           _ -> raise ("Could not match type when assigning variable "^name)
    | AssignStruct (name, field, exp) ->
         let ((exptype, sexp), _) = expr env exp in
         let t = type_of_field name field env.varmap in
         match match_type t exptype with
           TMatch -> ((t, SAssignStruct(name, field, sexp)), env)
           _ -> raise ("Could not match type when assigning field "^name^"."^field)
    | IntLit i -> ((Int, SIntLit i), env)
    | FloatLit f -> ((Float, SFloatLit i), env)
    | BoolLit b -> ((Bool, SBoolLit b), env)
    | _ -> raise ("There is an unsupported expression in this program")

  (* check a single statement and update the environment *)
  let rec stmt env = function
      Expression(e) -> let (se, en) = expr env e in (SExpression (se), en)
    | Typedef(td) -> (STypedef(td), {env with typemap = add_typedef td env.typemap})
    | Import -> raise ("Import statements not currently supported") (* TODO *)

  let rec stmts env = function 
    hd :: tl -> let (st, en) = stmt hd in st :: stmts en tl
    _ -> []
  in stmts prog global_env
