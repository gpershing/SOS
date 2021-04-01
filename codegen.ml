(* Code generation: translate takes a semantically checked AST and
produces LLVM IR *)

module L = Llvm
open Sast 

module StringMap = Map.Make(String)

type environment = {
ebuilder : L.llbuilder
evars : L.llvalue StringMap.t; (* The storage associated with a given var *)
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
       add_variable env nm var ;
       expr env (t, SAssign(nm, ex)) (* Bootstrap off Assign *)

   | SFxnDef(ty, nm, args, ex) ->
       let formal_types =
           Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) args) in
       let ftype = L.function_type (ltype_of_typ ty) formal_types in
       let decl = L.define_function nm ftype the_module in
       let new_builder = L.builder_at_end context (L.entry_block decl) in

       let new_env = List.fold_left2 add_formal env args (
              Array.to_list (L.params decl)) in
       let (lv, ret_env) = expr {new_env with ebuilder=new_builder} ex in
       
       (* End with a return statement *)
       L.build_ret lv ret_env.ebuilder;
       (* Return the old environment *) (* TODO update env's fxns *)
       decl, env 

     (* Assignments *)
   | SAssign(nm, ex) ->
       let ex' = expr env ex in
       let (lv, _) = ex' in
       ignore(L.build_store lv (get_variable env nm) env.ebuilder); ex'

     (* Function application *)
   | SFxnApp("printf", SOrderedFxnArgs([e])) ->
      let float_format_str =
       L.build_global_stringptr "%g\n" "fmt" env.ebuilder in
      let arg, _ = expr env e in 
      L.build_call printf_func [| float_format_str ; arg |]
        "printf" env.ebuilder, env
   
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
     let start_env = { ebuilder = builder; evars = StringMap.empty } in
     let end_env = List.fold_left build_stmt env stmts in
     (* Add a return statement *)
     L.build_ret (L.const_int i32_t 0) env.ebuilder    

   in
   ignore(build_main prog);
   the_module
