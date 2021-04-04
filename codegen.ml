(* Code generation: translate takes a semantically checked AST and
produces LLVM IR *)

module L = Llvm
open Ast
open Sast 

module StringMap = Map.Make(String)

type environment = {
ebuilder : L.llbuilder;
evars : L.llvalue StringMap.t; (* The storage associated with a given var *)
efxns : (L.llvalue * func_bind) StringMap.t;
ecurrent_fxn : L.llvalue (* The current function *)
}

(* translate : Sast.program -> Llvm.module *)
let translate prog =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "SOS" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context in

  (* Return the LLVM type for a SOS type *)
  let ltype_of_typ = function
      Int   -> i32_t
    | Bool  -> i1_t
    | Float -> float_t
    | Void  -> void_t
    | _ -> raise (Failure "Non-basic types not yet supported") (*TODO*)  

  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  (* Setup main function *)
  let main =
      L.define_function "main" (L.function_type i32_t [||]) the_module
  in

  (* Add a variable llvalue to environment.evars *)
  let add_variable env nm lv =
       {env with evars = StringMap.add nm lv env.evars }
  in
  
  (* Get a variable's llvalue from environment.evars *)
  let get_variable env nm = StringMap.find nm env.evars
  in

  (* Add a function declaration to environment.efxns *)
  let add_function env nm bind = 
      {env with efxns = StringMap.add nm bind env.efxns }
  in

  (* Gets a function's llvalue and fdecl from environment.evars *)
  let get_function env nm = StringMap.find nm env.efxns
  in

  (* Add a formal argument llvalue to environment.evars *)
  let add_formal env (ty, nm) param =
    L.set_value_name nm param;
    let local = L.build_alloca (ltype_of_typ ty) nm env.ebuilder in
    ignore (L.build_store param local env.ebuilder);
    add_variable env nm local
  in

    (* Add a new type definition, return ? *)
    let add_typedef = function 
      SAlias(_) -> raise (Failure "Alias not yet supported")
    | SStructDef(_) -> raise (Failure "Struct def not yet supported")
    in

   (* Construct code for a binop
      Return its llvalue and the updated environment *)
   let binop_expr env typ ll1 op ll2 = (match op with
      Add -> (match typ with
        Int -> L.build_add
      | Float -> L.build_fadd
      | _ -> raise (Failure "That type not supported for Add")
      )
    | Sub -> (match typ with
        Int -> L.build_sub
      | Float -> L.build_fsub
      | _ -> raise (Failure "That type not supported for Sub")
      )
    | _ -> raise (Failure "Operator not yet supported")
    ) ll1 ll2 "tmp" env.ebuilder, env 
   in

   (* Construct code for an expression
      Return its llvalue and the updated builder *)
   let rec expr env sexpr = 
     let (t, e) = sexpr in match e with
     (* Literals *)
     SIntLit(i) -> L.const_int i32_t i, env
   | SFloatLit(f) -> L.const_float_of_string float_t f, env
   | SBoolLit(b) -> L.const_int i1_t (if b then 1 else 0), env

     (* Access *)
   | SVar(nm) -> (L.build_load (get_variable env nm) nm env.ebuilder),env 

     (* Definitions *)
   | SVarDef(ty, nm, ex) -> 
       let var = L.build_alloca (ltype_of_typ ty) nm env.ebuilder in
       let env = add_variable env nm var in
       expr env (t, SAssign(nm, ex)) (* Bootstrap off Assign *)

   | SFxnDef(ty, nm, args, ex) ->
       let formal_types =
           Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) args) in
       let ftype = L.function_type (ltype_of_typ ty) formal_types in
       let decl = L.define_function nm ftype the_module in
       let new_builder = L.builder_at_end context (L.entry_block decl) in
       let bind = decl, { ftype = ty; formals = args } in

       let new_env = List.fold_left2 add_formal env args (
              Array.to_list (L.params decl)) in
       let new_env = add_function new_env nm bind in
       let new_env = {new_env with ebuilder = new_builder; ecurrent_fxn=decl} in
       let (lv, ret_env) = expr new_env ex in
       
       (* End with a return statement *)
       ignore (L.build_ret lv ret_env.ebuilder);
       (* Add this function to the returned environment *)
       decl, add_function env nm bind

     (* Assignments *)
   | SAssign(nm, ex) ->
       let ex' = expr env ex in
       let (lv, env) = ex' in
       ignore(L.build_store lv (get_variable env nm) env.ebuilder); ex'

     (* Operators *)
   | SBinop(exp1, op, exp2) ->
       (match op with
         (* Sequencing deals with environment differently than other binops*)
         Seq -> raise (Failure "Sequencing not yet supported")
       | _ -> 
         let (ll1, env) = expr env exp1 in
         let (ll2, env) = expr env exp2 in
         binop_expr env t ll1 op ll2
       )

     (* Function application *)
    (* Special functions *)
   | SFxnApp("printf", SOrderedFxnArgs([e])) ->
      let float_format_str =
       L.build_global_stringptr "%g\n" "fmt" env.ebuilder in
      let arg, env  = expr env e in 
      L.build_call printf_func [| float_format_str ; arg |]
        "printf" env.ebuilder, env

    (* General functions *)
   | SFxnApp(nm, SOrderedFxnArgs(args)) -> 
      let (fdef, fdecl) = get_function env nm in
      (* Get llvalues of args and accumualte env *)
      let (llargs_rev, env) = List.fold_left
        (fun (l, en) a -> let (ll, e) = expr env a in (ll::l, e))
        ([], env) args in
      let llargs = List.rev llargs_rev in
      let result = (match fdecl.ftype with
                      Void -> ""
                    | _ -> nm ^ "_result") in
      L.build_call fdef (Array.of_list llargs) result env.ebuilder, env

    (* Control flow *)
   | SIfElse (eif, ethen, eelse) ->
      let (cond, inenv) = expr env eif in
      (* Memory to store the value of this expression *)
      let ret = L.build_alloca (ltype_of_typ t) "if_tmp" env.ebuilder in
      let merge_bb = L.append_block context "merge" env.ecurrent_fxn in
      let then_bb = L.append_block context "then" env.ecurrent_fxn in
      let else_bb = L.append_block context "else" env.ecurrent_fxn in
      ignore (L.build_cond_br cond then_bb else_bb env.ebuilder);

      let (thenv, then_env) = expr {inenv with ebuilder=(L.builder_at_end context then_bb)} ethen in
      ignore (L.build_store thenv ret then_env.ebuilder);
      ignore (L.build_br merge_bb then_env.ebuilder);

      let (elsev, else_env) = expr {inenv with ebuilder=(L.builder_at_end context else_bb)} eelse in
      ignore (L.build_store elsev ret else_env.ebuilder);
      ignore (L.build_br merge_bb else_env.ebuilder);

      let env ={env with ebuilder=(L.builder_at_end context merge_bb)} in
      (L.build_load ret "if_tmp" env.ebuilder), env
   
   | _ -> raise (Failure "Found an unsupported expression")
   in

   (* Builds an SOS statement and returns the updated environment *)
   let build_stmt env = function
     STypeDef(td) -> add_typedef td; env
   | SExpression(ex) -> let (_, env) = expr env ex 
     in env
   | SImport(im) -> raise (Failure "Import not yet supported") (*TODO*)
   in
  
   (* Build the main function, the entry point for the whole program *)
   let build_main stmts = 
     (* Init the builder at the beginning of main() *)
     let builder = L.builder_at_end context (L.entry_block main) in
     (* Use the builder to add the statements of main() *)
     let start_env = { ebuilder = builder; evars = StringMap.empty;
       efxns = StringMap.empty; ecurrent_fxn = main } in
     let end_env = List.fold_left build_stmt start_env stmts in
     (* Add a return statement *)
     L.build_ret (L.const_int i32_t 0) end_env.ebuilder    

   in
   ignore(build_main prog);
   the_module
