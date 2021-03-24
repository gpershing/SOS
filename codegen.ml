(* Code generation: translate takes a semantically checked AST and
produces LLVM IR *)

module L = Llvm
open Sast 

module StringMap = Map.Make(String)

type environment = {
evars : L.llvalue StringMap.t; (* The storage associated with a given var *)
}

(* translate : Sast.program -> Llvm.module *)
let translate prog =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "SOS" in

  (* Pointer to the program environment *)
  let env = ref { evars = StringMap.empty } in

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
  let env_add_variable nm lv =
      env := {!env with evars = StringMap.add nm lv !env.evars }
  in
  
  (* Get a variable's llvalue from environment.evars *)
  let env_get_variable nm = StringMap.find nm !env.evars
  in


(* unmodified code till line 95


(* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

*)







(* builder is defined above...needs to define env? *)

    (* Add a new type definition, return ? *)
    let add_typedef = function 
      SAlias(_) -> raise (Failure "Alias not yet supported")
    | SStructDef(_) -> raise (Failure "Struct def not yet supported")
    in

   (* Construct code for an expression
      Return its llvalue and the updated builder *)
   let rec expr builder sexpr = 
     let (t, e) = sexpr in match e with
     (* Literals *)
     SIntLit(i) -> L.const_int i32_t i, builder
   | SFloatLit(f) -> L.const_float_of_string float_t f, builder
   | SBoolLit(b) -> L.const_int i1_t (if b then 1 else 0), builder

     (* Access *)
   | SVar(nm) -> (L.build_load (env_get_variable nm) nm builder), builder

     (* Definitions *)
   | SVarDef(ty, nm, ex) -> 
       let var = L.build_alloca (ltype_of_typ ty) nm builder in
       env_add_variable nm var ;
       expr builder (t, SAssign(nm, ex)) (* Bootstrap off Assign *)

     (* Assignments *)
   | SAssign(nm, ex) ->
       let ex' = expr builder ex in
       let (lv, _) = ex' in
       ignore(L.build_store lv (env_get_variable nm) builder); ex'

     (* Function application *)
   | SFxnApp("printf", SOrderedFxnArgs([e])) ->
      let float_format_str =
       L.build_global_stringptr "%g\n" "fmt" builder in
      let arg, _ = expr builder e in 
      L.build_call printf_func [| float_format_str ; arg |]
        "printf" builder, builder
   
   | _ -> raise (Failure "Found an unsupported expression")
   in

   (* Builds an SOS statement and returns the updated builder *)
   let build_stmt builder = function
     STypeDef(td) -> add_typedef td; builder (* Builder unchanged *)
   | SExpression(ex) -> let (_, builder) = expr builder ex 
     in builder
   | SImport(im) -> raise (Failure "Import not yet supported") (*TODO*)
   in
  
   (* Build the main function, the entry point for the whole program *)
   let build_main stmts = 
     (* Init the builder at the beginning of main() *)
     let builder = L.builder_at_end context (L.entry_block main) in
     (* Use the builder to add the statements of main() *)
     let builder = List.fold_left build_stmt builder stmts in
     (* Add a return statement *)
     L.build_ret (L.const_int i32_t 0) builder    

   in
   ignore(build_main prog);
   the_module
