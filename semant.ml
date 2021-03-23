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

let raisestr s = raise (Failure s) 

let check prog =

  (* add built-in function such as basic printing *)
  let built_in_decls = (
    let add_bind map (name, ty) = StringMap.add name {
      ftype = Void;
      formals = [(ty, "x")] (* maybe need locals here? *)} map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("printb", Bool);
			                         ("printf", Float)] )
  in

  (* add built-in types such as int, float *)
  let built_in_types = (
    let add_type map (name, ty) = StringMap.add name ty map
    in List.fold_left add_type StringMap.empty [("int", Int); ("bool", Bool); ("float", Float); ("void", Void)] )
  in

  (* Initial environment containing built-in types and functions *)
  let global_env = { typemap = built_in_types; varmap = StringMap.empty; funcmap =  built_in_decls }
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
      else raisestr ("Could not resolve type id "^s)
  | ArrayTypeID(s) -> Array(resolve_typeid s map)
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
    else raisestr ("Unknown variable name "^s)
  in

  (* function to lookup the type of a struct field *)
  let type_of_field s f map = 
    let stype = type_of_id s map in
    match stype with
      Struct(sargs) -> 
      let rec find_field f = function
        (ft, fn) :: tl -> if fn = f then ft else find_field f tl
        | _ -> raisestr ("Could not find field "^f)
      in find_field f sargs
     | _ -> raisestr ("Cannot access fields for a non-struct variable")
  in

  (* get the signature of a function *)
  let sig_of_func f map = 
    if StringMap.mem f map then StringMap.find f map
    else raisestr ("Unknown function name "^f)
  in

  let add_typedef td map =
    match td with
      Alias(nm, t) -> if StringMap.mem nm map
        then raisestr ("Cannot create an alias with preexisting name " ^ nm)
        else StringMap.add nm (resolve_typeid t map) map
    | StructDef(nm, l) -> raisestr ("Struct def not yet supported") (* TODO *)
  in

  let rec add_formals args vmap tmap = match args with
    (typ, nm) :: tl -> add_formals tl (StringMap.add nm (resolve_typeid typ tmap) vmap) tmap
    | _ -> vmap
  in

  (* maybe it is better do define check funcs for each
     alias, struct and expr here *)
  let rec expr env = function
     
      VarDef (tstr, name, exp) -> 
        let (sexp, _) = expr env exp in
        let (exptype, _) = sexp in
        let t = resolve_typeid tstr env.typemap in
        (match match_type t exptype with
          TMatch -> ((t, SVarDef(tstr, name, sexp)),
              { env with varmap = StringMap.add name t env.varmap } )
          (* TODO may want to deal with overriding variables differently *)
        |  _ -> raisestr ("Could not match type when defining variable " ^ name))

    | FxnDef (tstr, name, args, exp) ->
        let t = resolve_typeid tstr env.typemap in
        let sargs = List.map
          (fun (typ, nm) -> (resolve_typeid typ env.typemap, nm)) args
        in
        let newfxnmap = StringMap.add
           name { ftype = t; formals = sargs} env.funcmap in
        let ((exptype, sx), _) = expr { env with
           funcmap = newfxnmap; 
           varmap = add_formals args env.varmap env.typemap; } exp
        in
        (match match_type t exptype with
          TMatch -> ((t, SFxnDef(tstr, name, sargs, (exptype, sx))),
                  { env with funcmap = newfxnmap })
          | _ -> raisestr ("Incorrect return type for function " ^ name))

    | Assign (name, exp) ->
         let ((exptype, sexp), _) = expr env exp in
         let t = type_of_id name env.varmap in
         (match match_type t exptype with
           TMatch -> ((t, SAssign(name, (exptype, sexp))), env)
         | _ -> raisestr ("Could not match type when assigning variable "^name))

    | AssignStruct (name, field, exp) ->
         let ((exptype, sexp), _) = expr env exp in
         let t = type_of_field name field env.varmap in
         (match match_type t exptype with
           TMatch -> ((t, SAssignStruct(name, field, (exptype, sexp))), env)
         |  _ -> raisestr ("Could not match type when assigning field "^name^"."^field))

    | Uop(_) -> raisestr ("Unary operations not yet supported") (* TODO *)

    | Binop(_) -> raisestr ("Binary operations not yet supported") (* TODO *)

    | FxnApp (name, args) -> 
         let fxn = sig_of_func name env.funcmap in
         (match args with
           OrderedFxnArgs(exps) ->
             let rec check_args sigl expl env = match expl with
               hd :: tl -> let ((exptype, sexp), _) = expr env hd in
                 (match sigl with
                   (typ, nm) :: sigtl -> (match match_type typ exptype with
                       TMatch -> (exptype, sexp) :: check_args sigtl tl env
                     | _ -> raisestr ("Could not match type of argument "^nm))
                   | _ -> raisestr ("Too many arguments for function signature"))
             | [] -> if sigl = [] then [] else raisestr ("Function currying not yet supported")
           in ((fxn.ftype, SFxnApp(name, SOrderedFxnArgs(check_args fxn.formals exps env))), env) 
         | NamedFxnArgs(_) -> raisestr ("Named function arguments not yet supported") (* TODO *)
         )

    | IfElse (eif, ethen, eelse) -> 
        let ((exptype, sexp), _) = expr env eif in
        (match match_type exptype Bool with
          TMatch -> let ((thentype, thenexp), _) = expr env ethen in
            let ((elsetype, elseexp), _)  = expr env eelse in
            (match match_type thentype elsetype with
              TMatch -> ((thentype, SIfElse((exptype, sexp), (thentype, thenexp), (elsetype, elseexp))), env)
            | _ -> raisestr ("Could not reconcile types of then and else clauses"))
        | _ -> raisestr ("Could not resolve if condition to a bool"))

    | ArrayCon l -> (match l with
      hd :: tl ->
        let ((exptype, sexp), _) = expr env hd in 
        ((Array(exptype), SArrayCon((exptype, sexp) :: List.map
          (fun ex -> let ((et, se), _) = expr env ex in
           match match_type exptype et with TMatch -> (et, se) 
           | _ -> raisestr ("Cannot make an array literal out of elements of different types")
          )
          tl)), env)
      | [] -> ((EmptyArray, SArrayCon([])), env) (* TODO: make appropriate array cast *)
      )

    | AnonStruct l -> 
      let rec create_anon_struct n = function
        e :: tl -> let ((exptype, sexp), _) = expr env e in
          let (typel, expl) = create_anon_struct (n+1) tl in
          ((exptype, "x"^string_of_int n) :: typel, (exptype, sexp) :: expl)
      | _ -> [], []
      in
      let (typel, expl) = create_anon_struct 1 l in
      ((Struct(typel), SAnonStruct(expl)), env)

    | NamedStruct (name, l)  ->
      let st = resolve_typeid (TypeID(name)) env.typemap in
      (match st with
        Struct(sargs) -> 
        let rec create_named_struct argl = function
          e :: tl -> let ((exptype, sexp), _) = expr env e in
            (match argl with (t, nm) :: argtl ->
              (match match_type t exptype with
                 TMatch -> (exptype, sexp) :: create_named_struct argtl tl
               | _ -> raisestr ("Could not resolve type of struct field "^nm))
            | _ -> raisestr ("Too many arguments for struct "^name))
          | [] -> (match argl with
            [] -> []
           | _ -> raisestr ("Not enough arguments for struct "^name))
        in
        let sexprs = create_named_struct sargs l
        in ((st, SNamedStruct(name, sexprs)), env)
      | _ -> raisestr ("Cannot resolve the struct name "^name)
     )

    | Var i -> ((type_of_id i env.varmap, SVar(i)), env)
    | StructField (nm, fl) -> ((type_of_field nm fl env.varmap, SStructField(nm ,fl)), env)
   

    | IntLit i -> ((Int, SIntLit i), env)
    | FloatLit f -> ((Float, SFloatLit f), env)
    | BoolLit b -> ((Bool, SBoolLit b), env)
    | _ -> raisestr ("There is an unsupported expression in this program")
  in

  let make_stypedef env = function
      Alias(nm, tp) -> SAlias(nm, resolve_typeid tp env.typemap)
    | StructDef(nm , l) -> SStructDef(nm, List.map (fun (t, i) -> (resolve_typeid t env.typemap, i)) l)
  in

  (* check a single statement and update the environment *)
  let rec stmt env = function
      Expression(e) -> let (se, en) = expr env e in (SExpression (se), en)
    | Typedef(td) -> (STypeDef(make_stypedef env td), {env with typemap = add_typedef td env.typemap})
    | Import(_) -> raisestr ("Import statements not currently supported") (* TODO *)
  in

  let rec stmts env = function 
    hd :: tl -> let (st, en) = stmt env hd in st :: stmts en tl
  | _ -> []
  in stmts global_env prog
