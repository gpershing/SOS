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
  let i32_t      = L.i32_type     context
  and i8_t       = L.i8_type      context
  and i1_t       = L.i1_type      context
  and float_t    = L.double_type  context
  and void_t     = L.void_type    context 
  and ptr_t      = L.pointer_type 
  and struct_t   = L.struct_type  context in

  (* Convenient notation for GEP instructions, etc *)
  let l0         = L.const_int i32_t 0 in
  let l1         = L.const_int i32_t 1 in

  (* Return the LLVM type for a SOS type *)
  let rec ltype_of_typ = function
      Int      -> i32_t
    | Bool     -> i1_t
    | Float    -> float_t
    | Void     -> void_t
    (* An array is a pointer to a struct containing an array (as a pointer)
      and its length, an int *)
    | Array(t) -> ptr_t (struct_t [|ptr_t (ltype_of_typ t); i32_t|])
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

   (* Operator Maps *)
   let opstr = function
     Add -> "Add"
   | Sub -> "Sub"
   | Mul -> "Mul"
   | Div -> "Div"
   | Mod -> "Mod"
   | Pow -> "Pow"
   | Eq -> "Eq"
   | Neq -> "Neq"
   | Less -> "Less"
   | Greater -> "Greater"
   | LessEq -> "LessEq"
   | GreaterEq -> "GreaterEq"
   | And -> "And"
   | Or -> "Or" 
   | Of -> "Of"
   | Concat -> "Concat"
   | Seq -> "Seq"
   in

   let make_opmap l =
     List.fold_left (fun map (op, fxn) -> StringMap.add (opstr op) fxn map)
      StringMap.empty l
   in

   let int_map = make_opmap
   [(Add, L.build_add); (Sub, L.build_sub);
    (Mul, L.build_mul); (Div, L.build_sdiv);
    (Eq, L.build_icmp L.Icmp.Eq); (Neq, L.build_icmp L.Icmp.Ne);
    (Less, L.build_icmp L.Icmp.Slt); (Greater, L.build_icmp L.Icmp.Sgt);
    (LessEq, L.build_icmp L.Icmp.Sle); (GreaterEq, L.build_icmp L.Icmp.Sge)
     ]
   in
   
   let float_map = make_opmap
   [(Add, L.build_fadd); (Sub, L.build_fsub);
    (Mul, L.build_fmul); (Div, L.build_fdiv);
    (Eq, L.build_fcmp L.Fcmp.Oeq); (Neq, L.build_fcmp L.Fcmp.One);
    (Less, L.build_fcmp L.Fcmp.Olt); (Greater, L.build_fcmp L.Fcmp.Ogt);
    (LessEq, L.build_fcmp L.Fcmp.Ole); (GreaterEq, L.build_fcmp L.Fcmp.Oge)
     ]
   in

   (* Array operataions *)
   let build_array_len env lv = (* Re-used in build_of and expr *)
     let lenref = L.build_gep lv [|l0; l1|]
      ("lengep") env.ebuilder in
     L.build_load lenref ("len") env.ebuilder, env
   in

   let build_of t1 t2 ll1 ll2 env = 
     (* Get ll2's length *)
     let (len, env) = build_array_len env ll2 in
    
     (* Compute new length *)
     let n = L.build_mul ll1 len "oflen" env.ebuilder in
     (* Pre-GEP the array *)
     let old_data_ref = L.build_gep ll2 [|l0; l0|] ("oldrf") env.ebuilder in
     let old_data = L.build_load old_data_ref ("olddata") env.ebuilder in
     
     (* Create a new array *)
     let el_typ = match t2 with Array(et) -> et | _ -> Void in
     let data = L.build_array_alloca (ltype_of_typ el_typ) n
       "arrdata" env.ebuilder in
     (* Set up loop *)
     let i_addr = L.build_alloca i32_t "i" env.ebuilder in
     ignore (L.build_store l0 i_addr env.ebuilder);
     let j_addr = L.build_alloca i32_t "j" env.ebuilder in
     ignore (L.build_store l0 j_addr env.ebuilder); 
     let loop_bb = L.append_block context "loop" env.ecurrent_fxn in
     let inner_bb = L.append_block context "inner" env.ecurrent_fxn in
     let continue_bb = L.append_block context "continue" env.ecurrent_fxn in
     ignore (L.build_br inner_bb env.ebuilder);

     (* Inner loop *)
     let builder = L.builder_at_end context inner_bb in
     let i = L.build_load i_addr "i" builder in
     let j = L.build_load j_addr "j" builder in
     let elref = L.build_gep old_data [|j|] ("elref") builder in
     let el = L.build_load elref ("el") builder in
     let newref = L.build_gep data [|i|] ("newref") builder in
     ignore(L.build_store el newref builder); 
     ignore(L.build_store (L.build_add j l1 "j" builder) j_addr builder);
     ignore(L.build_store (L.build_add i l1 "i" builder) i_addr builder);
     let j = L.build_load j_addr "j" builder in
     ignore(L.build_cond_br (L.build_icmp L.Icmp.Slt j len "tmp" builder)
       inner_bb loop_bb builder);

     (* Outer loop *)
     let builder = L.builder_at_end context loop_bb in
     let i = L.build_load i_addr "i" builder in
     ignore (L.build_store l0 j_addr builder);
     ignore (L.build_cond_br (L.build_icmp L.Icmp.Slt i n "tmp" builder)
       inner_bb continue_bb builder);

     (* Continue *)
     let builder = L.builder_at_end context continue_bb in
     let env = { env with ebuilder = builder } in
     (* Create array struct *)
     let arr_struct = L.build_alloca (L.element_type (ltype_of_typ t2))
       "arr" env.ebuilder in
     let data_addr = L.build_gep arr_struct [|l0; l0|] "datafield"
       env.ebuilder in
     ignore (L.build_store data data_addr env.ebuilder);
     let len_addr = L.build_gep arr_struct [|l0; L.const_int i32_t 1|]
       "lenfield" env.ebuilder in
     ignore (L.build_store n len_addr env.ebuilder);

     arr_struct, env
   in

   let build_concat t1 t2 ll1 ll2 env =
     (* Get lengths *)
     let (len1, _) = build_array_len env ll1 in
     let (len2, _) = build_array_len env ll2 in
     let n = L.build_add len1 len2 "n" env.ebuilder in
     (* Pre-GEP the arrays *)
     let data_ref1 = L.build_gep ll1 [|l0; l0|] ("dataref1") env.ebuilder in
     let data_ref2 = L.build_gep ll2 [|l0; l0|] ("dataref2") env.ebuilder in
     let data1 = L.build_load data_ref1 ("data1") env.ebuilder in
     let data2 = L.build_load data_ref2 ("data2") env.ebuilder in
     (* Create a new array *)
     let el_typ = match t2 with Array(et) -> et | _ -> Void in
     let data = L.build_array_alloca (ltype_of_typ el_typ) n
       "data" env.ebuilder in 
     (* Set up loop *)
     let i_addr = L.build_alloca i32_t "i" env.ebuilder in
     ignore (L.build_store l0 i_addr env.ebuilder);
     let j_addr = L.build_alloca i32_t "j" env.ebuilder in
     ignore (L.build_store l0 j_addr env.ebuilder);

     let loop1 = L.append_block context "loop1" env.ecurrent_fxn in
     let inbtw = L.append_block context "inbtw" env.ecurrent_fxn in
     let loop2 = L.append_block context "loop2" env.ecurrent_fxn in
     let contb = L.append_block context "contb" env.ecurrent_fxn in
     ignore (L.build_br loop1 env.ebuilder);

     let make_concat_loop sbb tbb from_data len =
       let builder = L.builder_at_end context sbb in
       let i = L.build_load i_addr "i" builder in
       let j = L.build_load j_addr "j" builder in
       let elref = L.build_gep from_data [|j|] "elref" builder in
       let el = L.build_load elref "el" builder in
       let newref = L.build_gep data [|i|] "newref" builder in
       ignore (L.build_store el newref builder);
       ignore (L.build_store (L.build_add j l1 "tmp" builder)
               j_addr builder);
       ignore (L.build_store (L.build_add i l1 "tmp" builder)
               i_addr builder);
       let j = L.build_load j_addr "j" builder in
       ignore (L.build_cond_br 
               (L.build_icmp L.Icmp.Slt j len "tmp" builder)
               sbb tbb builder)
     in
     (* Loop 1 *)
     make_concat_loop loop1 inbtw data1 len1 ;
     
     let builder = L.builder_at_end context inbtw in
     ignore(L.build_store l0 j_addr builder);
     ignore(L.build_br loop2 builder);
     (* Loop 2 *)
     make_concat_loop loop2 contb data2 len2 ;

     (* Continue *)
     let builder = L.builder_at_end context contb in
     let env = { env with ebuilder = builder } in
     (* Create array struct *)
     let arr_struct = L.build_alloca (L.element_type (ltype_of_typ t2))
       "arr" env.ebuilder in
     let data_addr = L.build_gep arr_struct [|l0; l0|] "datafield"
       env.ebuilder in
     ignore (L.build_store data data_addr env.ebuilder);
     let len_addr = L.build_gep arr_struct [|l0; L.const_int i32_t 1|]
       "lenfield" env.ebuilder in
     ignore (L.build_store n len_addr env.ebuilder);

     arr_struct, env
   in

   let array_map = make_opmap
   [(Of, build_of); (Concat, build_concat)]
   in

   (* Construct code for an expression
      Return its llvalue and the updated builder *)
   let rec expr env sexpr = 
     let (t, e) = sexpr in match e with
     (* Literals *)
     SIntLit(i) -> L.const_int i32_t i, env
   | SFloatLit(f) -> L.const_float_of_string float_t f, env
   | SBoolLit(b) -> L.const_int i1_t (if b then 1 else 0), env
   | SArrayCon(expl) -> 
     let n = List.length expl in
     let el_typ = match t with Array(et) -> et | _ -> Void in
     (* Create data *)
     let data = L.build_array_alloca
       (ltype_of_typ el_typ)
       (L.const_int i32_t n) "arrdata" env.ebuilder in 
     let (_, env) = List.fold_left
       (fun (n, env) sx -> 
         let addr = L.build_gep data [|L.const_int i32_t n|]
         ("el"^(string_of_int n)) env.ebuilder in
         let (lv, env) = expr env sx in
         ignore (L.build_store lv addr env.ebuilder);
         (n+1, env) ) (0, env) expl in
     (* Create struct *)
     let arr_struct = L.build_alloca 
       (L.element_type (ltype_of_typ t)) "arr"
       env.ebuilder in
     let data_addr = L.build_gep arr_struct [|l0; L.const_int i32_t 0|] "datafield"
       env.ebuilder in
     ignore (L.build_store data data_addr env.ebuilder);
     let len_addr = L.build_gep arr_struct [|l0; L.const_int i32_t 1|]
       "lenfield" env.ebuilder in
     ignore (L.build_store (L.const_int i32_t n) len_addr env.ebuilder);

     arr_struct, env

     (* Access *)
   | SVar(nm) -> (L.build_load (get_variable env nm) nm env.ebuilder),env 
   | SArrayAccess(nm, idx) -> 
     let (idx_lv, env) = expr env idx in
     (* Access the struct pointer, then the field *)
     let arr = L.build_load (get_variable env nm) "arr" env.ebuilder in
     let elref = L.build_gep arr [|l0; l0|]
       (nm^"dataref") env.ebuilder in
     (* Access data *)
     let d = L.build_load elref (nm^"data") env.ebuilder in
     let valref = L.build_gep d [|idx_lv|] (nm^"elref") env.ebuilder in
     L.build_load valref (nm^"el") env.ebuilder, env

   | SArrayLength (nm) -> build_array_len env
       (L.build_load (get_variable env nm) nm env.ebuilder)

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
         let (t1, _) = exp1 in let (t2, _) = exp2 in
         let (ll1, env) = expr env exp1 in
         let (ll2, env) = expr env exp2 in
(*         binop_expr env t t1 t2 ll1 op ll2 *)
         (match (t1, t2) with
           (Int, Int) -> StringMap.find (opstr op) int_map ll1 ll2 "tmp"
             env.ebuilder, env
         | (Float, Float) -> StringMap.find (opstr op) float_map ll1 ll2
             "tmp" env.ebuilder, env
         | (_, Array(_)) -> 
           (match op with
             Of -> build_of t1 t2 ll1 ll2 env
           | Concat -> build_concat t1 t2 ll1 ll2 env
           | _ -> raise (Failure "Unsupported operation")
           )

         | _ -> raise (Failure "Unsupported operation")
        )       
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
      let (cond, env) = expr env eif in
      (* Memory to store the value of this expression *)
      let ret = (if t != Void then
        Some(L.build_alloca (ltype_of_typ t) "if_tmp" env.ebuilder)
        else None) in
      let merge_bb = L.append_block context "merge" env.ecurrent_fxn in
      let then_bb = L.append_block context "then" env.ecurrent_fxn in
      let else_bb = L.append_block context "else" env.ecurrent_fxn in
      ignore (L.build_cond_br cond then_bb else_bb env.ebuilder);

      let (thenv, then_env) = expr {env with ebuilder=(L.builder_at_end context then_bb)} ethen in
      (match ret with Some(rv) ->
        ignore (L.build_store thenv rv then_env.ebuilder)
        | None -> () );
      ignore (L.build_br merge_bb then_env.ebuilder);

      let (elsev, else_env) = expr {env with ebuilder=(L.builder_at_end context else_bb)} eelse in
      (match ret with Some(rv) ->
        ignore (L.build_store elsev rv else_env.ebuilder)
        | None -> () );
      ignore (L.build_br merge_bb else_env.ebuilder);

      let env ={env with ebuilder=(L.builder_at_end context merge_bb)} in
      let rv = match ret with
        Some(rv) -> rv
      | None -> L.const_int (ltype_of_typ Bool) 0
      in
      (L.build_load rv "if_tmp" env.ebuilder), env

    (* Type casting *)
   | SCast (ex) -> let t_to = t in let (t_from, _) = ex in 
      let normal_cast command = 
        let (lv, _) = expr env ex in
        (command lv (ltype_of_typ t_to) "cast" env.ebuilder, env)
      in
      let il i = (Int, SIntLit(i)) in
      let fl f = (Float, SFloatLit(f)) in
      (
      match (t_to, t_from) with
        (Int, Float)  -> normal_cast L.build_fptosi 
      | (Int, Bool)   -> expr env (Int, SIfElse(ex, il 1, il 0))
      | (Float, Int)  -> normal_cast L.build_sitofp
      | (Bool, Int)   -> expr env (Bool, SBinop(ex, Neq, il 0))
      | (Bool, Float) -> expr env (Bool, SBinop(ex, Neq, fl "0"))
      | _             -> raise (Failure "Unknown type cast")
      )
   
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
