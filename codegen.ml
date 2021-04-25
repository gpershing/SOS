(* Code generation: translate takes a semantically checked AST and
produces LLVM IR *)

module L = Llvm
open Ast
open Sast 

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type environment = {
ebuilder : L.llbuilder;
evars : L.llvalue StringMap.t; (* The storage associated with a given var *)
efxns : StringSet.t; (* Which names are original fxn definitions *)
esfxns : (L.llvalue * func_bind) StringMap.t; (* Decls for struct op fxns *)
ecurrent_fxn : L.llvalue; (* The current function *)
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
  and float_t    = L.float_type   context
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
    | Struct(l) -> ptr_t (struct_t
       (Array.of_list (List.map (fun (tid, _) -> ltype_of_typ tid) l)))
    | Func(l, r) -> ptr_t (L.function_type (ltype_of_typ r) (Array.of_list (List.map ltype_of_typ l)))
    | EmptyArray-> raise (Failure "Unexpected empty array")

  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  (* External Functions *)
  let add_external_fxn env (decl, name, binding_name) =
      let formals, rt = match decl with Func(formals, rt) -> formals, rt
        | _ -> raise (Failure "Unexpected external function decl") in
      let ftype = L.function_type (ltype_of_typ rt)
        (Array.of_list (List.map (fun t -> ltype_of_typ t) formals)) in
      let lldecl = L.declare_function binding_name ftype the_module in
      { env with evars = StringMap.add name lldecl env.evars ;
          efxns = StringSet.add name env.efxns }
  in

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
  let add_function env nm llv = 
      {env with evars = StringMap.add nm llv env.evars;
       efxns = StringSet.add nm env.efxns }
  in

  (* Add a formal argument llvalue to environment.evars *)
  let add_formal env (ty, nm) param =
    L.set_value_name nm param;
    let local = L.build_alloca (ltype_of_typ ty) nm env.ebuilder in
    ignore (L.build_store param local env.ebuilder);
    add_variable env nm local
  in

   (* Operator Maps *)
   let opstr = function
     Add -> "Add"
   | Sub -> "Sub"
   | Mul -> "Mul"
   | MMul-> "MMul"
   | Div -> "Div"
   | Mod -> "Mod"
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
    (Mod, L.build_srem);
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

   (* Creates a loop that increments i by 1 each iteration,
    * and branches if i >= length. 
    * i_addr : the address of the int to be iterated on
    * length : the value of i to branch at
    * nm     : the name of the iterated variable, to make the LL readable
    * build  : a llvalue -> llbuilder -> llbuilder that builds all the statements using i
    * loop_bb: the basic block to build in
    * end_bb : the basic block to go to *)
   let build_loop i_addr length nm loop_bb end_bb build = 
     let builder = L.builder_at_end context loop_bb in
     let i = L.build_load i_addr "i" builder in
     let builder = build i builder in
     ignore(L.build_store (L.build_add i l1 nm builder) i_addr builder);
     let i = L.build_load i_addr nm builder in
     ignore (L.build_cond_br (L.build_icmp L.Icmp.Slt i length "tmp" builder)
       loop_bb end_bb builder);
   in

   (* General shorthand *)
   let build_zero builder nm = 
     let addr = L.build_alloca i32_t nm builder in
     ignore(L.build_store l0 addr builder);
     addr
   in
   
   let build_param builder decl typ n nm =
       let param = (Array.get (L.params decl) n) in
       L.set_value_name nm param ;
       let local = L.build_alloca typ nm builder in
       ignore (L.build_store param local builder);
       L.build_load local nm builder
    in

   (* Array operataions *)
   let build_array_load data idx nm builder =
     let elref = L.build_gep data [|idx|] (nm^"ref") builder in
     L.build_load elref nm builder
   in

   let build_array_store data idx llv builder = 
     let ref = L.build_gep data [|idx|] "storeref" builder in
     ignore (L.build_store llv ref builder)
   in

   let build_array_struct lltyp data length nm env = 
     let arr_struct = L.build_malloc (L.element_type lltyp) nm env.ebuilder in
     let data_addr = L.build_gep arr_struct [|l0; l0|] (nm^"data") env.ebuilder in
     let len_addr  = L.build_gep arr_struct [|l0; l1|] (nm^"len")  env.ebuilder in
     ignore (L.build_store data  data_addr env.ebuilder);
     ignore (L.build_store length len_addr env.ebuilder);
     arr_struct
   in

   let build_array_data builder lv nm =
     let data_ref = L.build_gep lv [|l0; l0|] (nm^"ref") builder in
     L.build_load data_ref nm builder
   in

   let build_array_len builder lv nm =
     let lenref = L.build_gep lv [|l0; l1|]
      (nm^"ref") builder in
     L.build_load lenref nm  builder
   in

   let build_of t2 ll1 ll2 env = 
     (* Get ll2's length *)
     let len = build_array_len env.ebuilder ll2 "len" in
    
     (* Compute new length *)
     let n = L.build_mul ll1 len "oflen" env.ebuilder in
     (* Pre-GEP the array *)
     let old_data = build_array_data env.ebuilder ll2 "olddata" in
     
     (* Create a new array *)
     let el_typ = match t2 with Array(et) -> et | _ -> Void in
     let data = L.build_array_malloc (ltype_of_typ el_typ) n
       "arrdata" env.ebuilder in
     (* Set up loop *)
     let i_addr = build_zero env.ebuilder "i" in
     let j_addr = build_zero env.ebuilder "j" in
     let loop_bb = L.append_block context "loop" env.ecurrent_fxn in
     let inner_bb = L.append_block context "inner" env.ecurrent_fxn in
     let continue_bb = L.append_block context "continue" env.ecurrent_fxn in
     ignore (L.build_br inner_bb env.ebuilder);

     (* Inner loop *)
     build_loop j_addr len "j" inner_bb loop_bb
       (fun j builder -> 
        let i = L.build_load i_addr "i" builder in
        let el = build_array_load old_data j "el" builder in
        build_array_store data i el builder ;
        ignore(L.build_store (L.build_add i l1 "i" builder) i_addr builder);
        builder 
       ) ;

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
     let arr_struct = build_array_struct (ltype_of_typ t2) data n "arr" env in
     arr_struct, env
   in

   let build_concat t2 ll1 ll2 env =
     (* Get lengths *)
     let len1 = build_array_len env.ebuilder ll1 "len1" in
     let len2 = build_array_len env.ebuilder ll2 "len2" in
     let n = L.build_add len1 len2 "n" env.ebuilder in

     (* Pre-GEP the arrays *)
     let data1 = build_array_data env.ebuilder ll1 "data1" in
     let data2 = build_array_data env.ebuilder ll2 "data2" in

     (* Create a new array *)
     let el_typ = match t2 with Array(et) -> et | _ -> Void in
     let data = L.build_array_malloc (ltype_of_typ el_typ) n
       "data" env.ebuilder in 
     (* Set up loop *)
     let i_addr = build_zero env.ebuilder "i" in
     let j_addr = build_zero env.ebuilder "j" in

     let loop1 = L.append_block context "loop1" env.ecurrent_fxn in
     let inbtw = L.append_block context "inbtw" env.ecurrent_fxn in
     let loop2 = L.append_block context "loop2" env.ecurrent_fxn in
     let contb = L.append_block context "contb" env.ecurrent_fxn in
     ignore (L.build_br loop1 env.ebuilder);

     let make_concat_loop sbb tbb from_data len =
       build_loop j_addr len "j" sbb tbb
       (fun j builder ->
       let i = L.build_load i_addr "i" builder in
       let el = build_array_load from_data j "el" builder in
       build_array_store data i el builder ;
       ignore (L.build_store (L.build_add i l1 "tmp" builder)
               i_addr builder) ;
       builder )
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
     let arr_struct = build_array_struct (ltype_of_typ t2) data n "arr" env in
     arr_struct, env
   in

   (* Struct ops *)
   (* Finds the integer field index *)
   let find_field struct_typ field = 
       let sargl = match struct_typ with Struct(l) -> l | _  -> [] in
       let rec find_field_inner sargl field n = match sargl with sarg :: tl ->
         let (_, nm) = sarg in
         if nm = field then n else find_field_inner tl field n+1
       | _ -> raise (Failure "Field not found")
       in
       find_field_inner sargl field 0
   in

   let build_struct_field builder lv n nm =
     let ref = L.build_gep lv [|l0; L.const_int i32_t n|] (nm^"ref") builder in
     L.build_load ref nm builder
   in     

   let build_struct_store builder lv s_lv n = 
     let ref = L.build_gep s_lv [|l0; L.const_int i32_t n|] "ref" builder in
     ignore (L.build_store lv ref builder)
   in

   let dot_product stype slist env =
     let atype = match slist with
       (hd, _) :: _ -> if hd = Float then Float else Int
     | _ -> Float in
     let len = List.length slist in
     
     let name = "__dot"^(if atype=Float then "f" else "i")^(string_of_int len) in

     if StringMap.mem name env.esfxns then
       StringMap.find name env.esfxns, env
     else (* Make new function *)
       let ltype = ltype_of_typ stype in
       let formals = [|ltype; ltype|] in
       let ftype = L.function_type (ltype_of_typ atype) formals in
       let decl = L.define_function name ftype the_module in
       let builder = L.builder_at_end context (L.entry_block decl) in

       let bind = { ftype = atype; formals = [stype, "a"; stype, "b"] } in

       let a = build_param builder decl ltype 0 "a" in
       let b = build_param builder decl ltype 1 "b" in

       let res = L.build_alloca (ltype_of_typ atype) "dot" builder in
       let tmp = L.build_alloca (ltype_of_typ atype) "tmp" builder in
       ignore(L.build_store (if atype=Float then L.const_float (ltype_of_typ Float) 0.0 else L.const_int i32_t 0) res builder) ;
       let map = (if atype=Float then float_map else int_map) in
       let opmul = StringMap.find "Mul" map in
       let opadd = StringMap.find "Add" map in
       let rec dot_prod n = 
         if n < len then 
           let aval = build_struct_field builder a n "aval" in
           let bval = build_struct_field builder b n "bval" in
           ignore (L.build_store (opmul aval bval "tmp" builder) tmp builder);
           let tmpv = L.build_load tmp "tmp" builder in
           let resv = L.build_load res "res" builder in
           ignore (L.build_store (opadd tmpv resv "tmp" builder) res builder);
           dot_prod (n+1)
         else ()
       in
       dot_prod 0 ;

       (* Return *)
       let resv = L.build_load res "res" builder in
       ignore (L.build_ret resv builder) ;
       (decl, bind), { env with esfxns = StringMap.add name (decl, bind) env.esfxns }
   in

   let struct_sum stype slist op env =
     let atype = match slist with
       (hd, _) :: _ -> if hd = Float then Float else Int
     | _ -> Float in
     let len = List.length slist in
     
     let name = "__"^(if op=Add then "add" else "sub")^
      (if atype=Float then "f" else "i")^(string_of_int len) in

     if StringMap.mem name env.esfxns then
       StringMap.find name env.esfxns, env
     else (* Make new function *)
       let ltype = ltype_of_typ stype in
       let formals = [|ltype; ltype|] in
       let ftype = L.function_type ltype formals in
       let decl = L.define_function name ftype the_module in
       let builder = L.builder_at_end context (L.entry_block decl) in

       let bind = { ftype = stype; formals = [stype, "a"; stype, "b"] } in

       let a = build_param builder decl ltype 0 "a" in
       let b = build_param builder decl ltype 1 "b" in

       let struc = L.build_malloc (L.element_type ltype) "ret" builder in
       let map = (if atype=Float then float_map else int_map) in
       let sumop = StringMap.find (opstr op) map in
       let rec strsum n = 
         if n < len then 
           let aval = build_struct_field builder a n "aval" in
           let bval = build_struct_field builder b n "bval" in
           build_struct_store builder (sumop aval bval "tmp" builder) struc n;
           strsum (n+1)
         else ()
       in
       strsum 0 ;

       (* Return *)
       ignore (L.build_ret struc builder) ;
       (decl, bind), { env with esfxns = StringMap.add name (decl, bind) env.esfxns }
   in

   let struct_scale stype slist op env =
     let atype = match slist with
       (hd, _) :: _ -> if hd = Float then Float else Int
     | _ -> Float in
     let len = List.length slist in
     
     let name = "__"^(if op=Mul then "mul" else "div")^
      (if atype=Float then "f" else "i")^(string_of_int len) in

     if StringMap.mem name env.esfxns then
       StringMap.find name env.esfxns, env
     else (* Make new function *)
       let ltype = ltype_of_typ stype in
       let altype = ltype_of_typ atype in
       let formals = [|ltype; altype|] in
       let ftype = L.function_type ltype formals in
       let decl = L.define_function name ftype the_module in
       let builder = L.builder_at_end context (L.entry_block decl) in

       let bind = { ftype = stype; formals = [stype, "a"; stype, "b"] } in

       let a = build_param builder decl ltype 0 "a" in
       let b = build_param builder decl altype 1 "b" in

       let struc = L.build_malloc (L.element_type ltype) "ret" builder in
       let map = (if atype=Float then float_map else int_map) in
       let sumop = StringMap.find (opstr op) map in
       let rec strscl n = 
         if n < len then 
           let aref = L.build_gep a [|l0; L.const_int i32_t n|] "aref" builder in
           let aval = L.build_load aref "aval" builder in
           build_struct_store builder (sumop aval b "tmp" builder) struc n;
           strscl (n+1)
         else ()
       in
       strscl 0 ;

       (* Return *)
       ignore (L.build_ret struc builder) ;
       (decl, bind), { env with esfxns = StringMap.add name (decl, bind) env.esfxns }
   in
   
   let struct_eq stype slist op env =
     let atype = match slist with
       (hd, _) :: _ -> if hd = Float then Float else Int
     | _ -> Float in
     let len = List.length slist in

     let name = "__"^(if op=Eq then "eq" else "neq")^
       (if atype=Float then "f" else "i")^(string_of_int len) in

     if StringMap.mem name env.esfxns then
       StringMap.find name env.esfxns, env
     else (* Make new function *)
       let ltype = ltype_of_typ stype in
       let formals = [|ltype; ltype|] in
       let ftype = L.function_type (ltype_of_typ Bool) formals in
       let decl = L.define_function name ftype the_module in
       let builder = L.builder_at_end context (L.entry_block decl) in

       let bind = { ftype = Bool; formals =
           [stype, "a"; stype, "b"] } in

       let a = build_param builder decl ltype 0 "a" in
       let b = build_param builder decl ltype 1 "b" in

       let ret = L.build_alloca (ltype_of_typ Bool) "ret" builder in
       let eq = StringMap.find (opstr op) (if atype=Float then float_map
          else int_map) in
       let combine = if op=Eq then L.build_and else L.build_or in
       ignore(L.build_store (L.const_int i1_t (if op=Eq then 1 else 0)) ret builder);
       let rec streq n = 
         if n < len then
           let aval = build_struct_field builder a n "aval" in
           let bval = build_struct_field builder b n "bval" in
           let rval = L.build_load ret "rval" builder in
           ignore(L.build_store
             (combine rval (eq aval bval "eq" builder) "ret" builder) ret builder);
           streq (n+1)
         else ()
       in streq 0 ;

       (* Return *)
       let rv = L.build_load ret "rval" builder in
       ignore(L.build_ret rv builder) ;
       (decl, bind), { env with esfxns = StringMap.add name (decl, bind) env.esfxns }
   in

   let mat_mul rtype slist1 slist2 env =
     let atype = match slist1 with
       (hd, _) :: _ -> if hd = Float then Float else Int
     | _ -> Float in
     let size = List.length slist1 in
     let int_sqrt n = 
       let rec int_sqrt_inner n m = 
         if m * m = n then m
         else if m * m < n then int_sqrt_inner n (m+1)
         else raise (Failure "Unexpected struct size")
       in int_sqrt_inner n 1
     in
     let n = int_sqrt size in
     let m = List.length slist2 in

     let name = "__"^(if m=n then "vec" else "mat")^
       (if atype=Float then "f" else "i")^(string_of_int n) in
     if StringMap.mem name env.esfxns then
       StringMap.find name env.esfxns, env
     else (* Make new function *)
       let rltype = ltype_of_typ rtype in
       let ltype1 = ltype_of_typ (Struct(slist1)) in
       let ltype2 = ltype_of_typ (Struct(slist2)) in
       let altype = ltype_of_typ atype in
       let formals = [|ltype1; ltype2|] in
       let ftype = L.function_type rltype formals in
       let decl = L.define_function name ftype the_module in
       let builder = L.builder_at_end context (L.entry_block decl) in

       let bind =
        { ftype = rtype; formals = [Struct(slist1), "A"; Struct(slist2), "B"] } in
       
       let a = build_param builder decl ltype1 0 "A" in
       let b = build_param builder decl ltype2 1 "B" in

       let struc = L.build_malloc (L.element_type rltype) "ret" builder in
       let map = (if atype=Float then float_map else int_map) in
       let sumop = StringMap.find (opstr Add) map in
       let mulop = StringMap.find (opstr Mul) map in
       let height = n in
       let width = (if m=n then 1 else n) in
       let tmp = L.build_alloca altype "tmp" builder in
       let rec mmul i j =
         if i < width then
         if j < height then (
         ignore(L.build_store (if atype=Float then L.const_float altype 0.0 else L.const_int altype 0) tmp builder) ;
         let rec mmul_inner k = 
           if k < height then (
           let aval = build_struct_field builder a (j+k*n) "aval" in
           let bval = build_struct_field builder b (k+i*n) "bval" in
           let mval = mulop aval bval "tmp2" builder in
           let tval = L.build_load tmp "tval" builder in
           ignore (L.build_store (sumop mval tval "tmp" builder) tmp builder);
           mmul_inner (k+1))
           else ()
         in mmul_inner 0 ;
         let tval = L.build_load tmp "tval" builder in
         build_struct_store builder tval struc (j+i*n);
         mmul (i+1) j )
         else (* j >= height *) ()
         else (* i >= width *)  mmul 0 (j+1)
       in
       mmul 0 0 ;

       (* Return *)
       ignore (L.build_ret struc builder) ;
       (decl, bind), { env with esfxns = StringMap.add name (decl, bind) env.esfxns }
   in

   (* Binops *)
   let rec binop op rt t1 t2 ll1 ll2 env = 
     if op = Of then build_of t2 ll1 ll2 env
     else if op = Concat then build_concat t2 ll1 ll2 env
     else
     (match (t1, t2) with
       (Bool, Bool) ->
         let llop = match op with
           Or -> L.build_or
         | And -> L.build_and
         | _ -> raise(Failure "Unexpected boolean operator") in
         llop ll1 ll2 "tmp" env.ebuilder, env
     | (Int, Int) -> StringMap.find (opstr op) int_map ll1 ll2 "tmp"
         env.ebuilder, env
     | (Float, Float) -> StringMap.find (opstr op) float_map ll1 ll2
         "tmp" env.ebuilder, env
     | (Struct(l1), Struct(l2)) -> let (decl, _), env = 
       if op=Mul then dot_product t1 l1 env 
       else if op=MMul then mat_mul rt l1 l2 env 
       else if op=Eq || op=Neq then struct_eq t1 l1 op env
       else struct_sum t1 l1 op env in
       L.build_call decl [|ll1; ll2|] "result" env.ebuilder, env
     | (Struct(l1), _) -> let (decl, _), env = struct_scale t1 l1 op env 
       in L.build_call decl [|ll1; ll2|] "result" env.ebuilder, env
     | (Array(_), _) ->
       let len = build_array_len env.ebuilder ll1 "len" in
      
       (* Pre-GEP the array *)
       let argdata = build_array_data env.ebuilder ll1 "argdata" in
       let el_typ = match t1 with Array(et) -> et | _ -> Void in
     
       (* Create a new array *)
       let rtel_typ = match rt with Array(et) -> et | _ -> Void in
       let data = L.build_array_malloc (ltype_of_typ rtel_typ) len
         "arrdata" env.ebuilder in
       (* Set up loop *)
       let i_addr = build_zero env.ebuilder "i" in
       let loop_bb = L.append_block context "loop" env.ecurrent_fxn in
       let cont_bb = L.append_block context "cont" env.ecurrent_fxn in
       ignore (L.build_br loop_bb env.ebuilder);
       (* Build loop *)
       build_loop i_addr len "i" loop_bb cont_bb (
         fun i builder -> 
         let v = build_array_load argdata i "v" builder in
         let fenv = { env with ebuilder = builder } in
         let llv, fenv = binop op rtel_typ el_typ t2 v ll2 fenv in
         let builder = fenv.ebuilder in
         build_array_store data i llv builder;
         builder ) ;
       (* Continue *)
       let builder = L.builder_at_end context cont_bb in
       let env = { env with ebuilder = builder } in
       (* Create array struct *)
       let arr_struct = build_array_struct (ltype_of_typ rt) data len "arr" env in
       arr_struct, env

     | (_, Array(_)) -> 
       let len = build_array_len env.ebuilder ll2 "len" in
      
       (* Pre-GEP the array *)
       let argdata = build_array_data env.ebuilder ll2 "argdata" in
       let el_typ = match t2 with Array(et) -> et | _ -> Void in
     
       (* Create a new array *)
       let rtel_typ = match rt with Array(et) -> et | _ -> Void in
       let data = L.build_array_malloc (ltype_of_typ el_typ) len
         "arrdata" env.ebuilder in
       (* Set up loop *)
       let i_addr = build_zero env.ebuilder "i" in
       let loop_bb = L.append_block context "loop" env.ecurrent_fxn in
       let cont_bb = L.append_block context "cont" env.ecurrent_fxn in
       ignore (L.build_br loop_bb env.ebuilder);
       (* Build loop *)
       build_loop i_addr len "i" loop_bb cont_bb (
         fun i builder -> 
         let v = build_array_load argdata i "v" builder in
         let fenv = { env with ebuilder = builder } in
         let llv, fenv = binop op rtel_typ t1 el_typ ll1 v fenv in
         let builder = fenv.ebuilder in
         build_array_store data i llv builder;
         builder ) ;
       (* Continue *)
       let builder = L.builder_at_end context cont_bb in
       let env = { env with ebuilder = builder } in
       (* Create array struct *)
       let arr_struct = build_array_struct (ltype_of_typ rt) data len "arr" env in
       arr_struct, env
     | _ -> raise (Failure "Unsupported operation")
     )
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
     let data = L.build_array_malloc
       (ltype_of_typ el_typ)
       (L.const_int i32_t n) "arrdata" env.ebuilder in 
     let (_, env) = List.fold_left
       (fun (n, env) sx -> 
         let (lv, env) = expr env sx in
         build_array_store data (L.const_int i32_t n) lv env.ebuilder;
         (n+1, env) ) (0, env) expl in
     (* Create struct *)
     let arr_struct = build_array_struct (ltype_of_typ t) data
       (L.const_int i32_t n) "arr" env in
     arr_struct, env

   | SStruct(nm, expl) ->
     let struc = L.build_malloc (L.element_type (ltype_of_typ t)) nm env.ebuilder in
     let rec set_fields env n = function
       exp :: tl -> 
         let fieldaddr = L.build_gep struc [|l0; L.const_int i32_t n|]
           "fieldaddr" env.ebuilder in 
         let (lv, env) = expr env exp in
         ignore(L.build_store lv fieldaddr env.ebuilder);
         set_fields env (n+1) tl
       | [] -> env
     in
     let env = set_fields env 0 expl in
     struc, env

     (* Access *)
   | SVar(nm) -> 
     if StringSet.mem nm env.efxns then (* Global fxn name, don't load *)
     get_variable env nm, env
     else (* All other variables *) 
     (L.build_load (get_variable env nm) nm env.ebuilder),env

   | SArrayAccess(arr_exp, idx) -> 
     let (arr, env) = expr env arr_exp in
     let (idx_lv, env) = expr env idx in
     (* Access the struct pointer, then the field *)
     let elref = L.build_gep arr [|l0; l0|]
       ("dataref") env.ebuilder in
     (* Access data *)
     let d = L.build_load elref ("data") env.ebuilder in
     build_array_load d idx_lv "el" env.ebuilder, env

   | SArrayLength (arr_exp) -> let (arr, env) = expr env arr_exp in
     build_array_len env.ebuilder arr "len", env

   | SStructField (str_exp, fl) ->
       let (stype, _) = str_exp in
       let (struc, env) = expr env str_exp in
       let idx = find_field stype fl in
       let adr = L.build_gep struc [|l0; L.const_int i32_t idx|]
         "fieldadr" env.ebuilder in
         L.build_load adr (fl) env.ebuilder, env

     (* Definitions *)
   | SVarDef(ty, nm, ex) ->  
       let var = L.build_alloca (ltype_of_typ ty) nm env.ebuilder in
       let env = add_variable env nm var in
       expr env (t, SAssign(nm, ex)) (* Bootstrap off Assign *)

     (* Assignments *)
   | SAssign(nm, ex) ->
       let ex' = expr env ex in
       let (lv, env) = ex' in
       ignore(L.build_store lv (get_variable env nm) env.ebuilder); ex'

   | SAssignStruct (str_exp, fl, ex) ->
       let (stype, _) = str_exp in
       let (struc, env) = expr env str_exp in
       let ex' = expr env ex in
       let (lv, env) = ex' in
       (* Find field index *)
       let idx = find_field stype fl in
       build_struct_store env.ebuilder lv struc idx; ex'

   | SAssignArray (arr_exp, idx, ex) ->
       let (arr, env) = expr env arr_exp in
       let data = build_array_data env.ebuilder arr "dataref" in
       let (i, env) = expr env idx in
       let (el, env) = expr env ex in
       build_array_store data i el env.ebuilder; (el, env)

     (* Operators *)
   | SUop (op, exp) ->
       let (l, env) = expr env exp in
       (match op with
         Neg when t = Float -> L.build_fneg
       | Neg                -> L.build_neg
       | Not                -> L.build_not) l "tmp" env.ebuilder, env

   | SBinop(exp1, op, exp2) ->
       (match op with
         Seq ->
           let (_, env) = expr env exp1 in
           expr env exp2 
       | _ -> 
         let (t1, _) = exp1 in let (t2, _) = exp2 in
         let (ll1, env) = expr env exp1 in
         let (ll2, env) = expr env exp2 in
         binop op t t1 t2 ll1 ll2 env       
       )

     (* Function application *)
    (* Special functions *)
   | SFxnApp((_, SVar("printf")), [e]) -> 
      let float_format_str =
       L.build_global_stringptr "%g\n" "fmt" env.ebuilder in
      let arg, env  = expr env e in 
      L.build_call printf_func [| float_format_str ; arg |]
        "printf" env.ebuilder, env
   | SFxnApp((_, SVar("print")), [e]) ->
      let int_format_str = 
       L.build_global_stringptr "%d\n" "fmt" env.ebuilder in
      let arg, env = expr env e in
      L.build_call printf_func [| int_format_str ; arg |]
        "printf" env.ebuilder, env


   | SFxnApp((_, SVar("copy")), [e]) ->
      let (ctype, _) = e in
      let arg, env = expr env e in
      (
      match ctype with
        Array(atype)    -> 
          let n = build_array_len env.ebuilder arg "len"in
          (* Pre-GEP the array *)
          let cdata = build_array_data env.ebuilder arg "cdata" in
     
          (* Create a new array *)
          let data = L.build_array_malloc (ltype_of_typ atype) n
            "arrdata" env.ebuilder in
          (* Set up loop *)
          let i_addr = build_zero env.ebuilder "i" in
          let loop_bb = L.append_block context "loop" env.ecurrent_fxn in
          let continue_bb = L.append_block context "continue" env.ecurrent_fxn in
          ignore (L.build_br loop_bb env.ebuilder);

          (* Loop *)
          build_loop i_addr n "i" loop_bb continue_bb
            (fun i builder ->
             let el = build_array_load cdata i "el" builder in
             build_array_store data i el builder ; builder ) ;

         (* Continue *)
         let builder = L.builder_at_end context continue_bb in
         let env = { env with ebuilder = builder } in
         (* Create array struct *)
         let arr_struct = build_array_struct (ltype_of_typ t) data n
           "arr" env in
         arr_struct, env
          
      | Struct(sfields) ->
         let len = List.length sfields in
         let name = "__copy"^(string_of_int len) in

         let (decl, _), env = if StringMap.mem name env.esfxns
         then StringMap.find name env.esfxns, env
         else (* Make a new copy fxns *)
           let formals = [|ltype_of_typ ctype|] in
           let ftype = L.function_type (ltype_of_typ ctype) formals in
           let decl = L.define_function name ftype the_module in
           let builder = L.builder_at_end context (L.entry_block decl) in

           let bind = { ftype = ctype; formals = [ctype, "to_copy"] } in

           let tocopy = build_param builder decl (ltype_of_typ ctype) 0 "to_copy" in

           (* Create a new struct *)
           let struc = L.build_malloc (L.element_type (ltype_of_typ ctype))
             "struct" builder in
           let rec copy_struct n =
             if n < len then
             let fl = build_struct_field builder tocopy n "fl" in
             build_struct_store builder fl struc n;
             copy_struct (n+1)
             else ()
           in
           copy_struct 0 ;

           (* Return *)
           ignore (L.build_ret struc builder) ;
           (decl, bind), { env with esfxns = StringMap.add name (decl, bind) env.esfxns }
         in
         L.build_call decl [|arg|] "copied" env.ebuilder, env


      | _ -> raise (Failure "Copy constructor only works on reference types")
      )

   (* Free instruction *)
   | SFxnApp((_, SVar("free")), [e]) ->
      let (ctype, _) = e in
      let arg, env = expr env e in
      (
      match ctype with
        Array(_)    -> 
         (* Need to free data as well as the structure *)
         let dataref = build_array_data env.ebuilder arg "data" in
         ignore(L.build_free dataref env.ebuilder);
      | _ -> () ) ;
      (* Free structure *)
      ignore(L.build_free arg env.ebuilder) ;
      l0, env

    (* General functions *)
   | SFxnApp(fexp, args) ->  
      let fdef, env = expr env fexp in
      let (fxntype, _) = fexp in
      let _, rt = match fxntype with Func(l, t) -> l, t | _ -> 
        raise (Failure "Unexpected function type") in

      (* Get llvalues of args and accumualte env *)
      let (llargs_rev, env) = List.fold_left
        (fun (l, env) a -> let (ll, e) = expr env a in (ll::l, e))
        ([], env) args in
      let llargs = List.rev llargs_rev in
      let result = (match rt with
                      Void -> ""
                    | _ -> "fxn_result") in
      (* Normal function application *)
      L.build_call fdef (Array.of_list llargs) result env.ebuilder, env

    | SIterFxnApp(fexp, args) ->
      let fdef, env = expr env fexp in
      let (fxntype, _) = fexp in
      let fargs, rt = match fxntype with Func(l, t) -> l, t | _ -> 
        raise (Failure "Unexpected function type") in

      (* Get llvalues of args and accumualte env *)
      let (llargs_rev, env) = List.fold_left
        (fun (l, env) a -> let (ll, e) = expr env a in (ll::l, e))
        ([], env) args in
      let llargs = List.rev llargs_rev in
      let result = (match rt with
                      Void -> ""
                    | _ -> "fxn_result") in

      (* Iterated fxn application *)
      let arr_args = List.map2 (fun (ty, _) fty -> ty=Array(fty) )
           args fargs in
      let rec find_first bools = function 
        (hd :: tl) -> if List.hd bools then hd else find_first (List.tl bools) tl
      | _ -> raise (Failure "Unexpected arguments")
      in let first = find_first arr_args llargs in
      let len = build_array_len env.ebuilder first "len" in
      (* Pre-GEP all the arrays *)
      let datalist = List.map2
        (fun llarg b -> if b then 
          Some(build_array_data env.ebuilder llarg "data")
         else None) llargs arr_args in

      (* Create a new array *)
      let data = if t=Void then None else 
        Some(L.build_array_malloc (ltype_of_typ rt) len
       "arrdata" env.ebuilder) in
      (* Set up loop *)
      let i_addr = build_zero env.ebuilder "i" in
      let loop_bb = L.append_block context "loop" env.ecurrent_fxn in
      let cont_bb = L.append_block context "continue" env.ecurrent_fxn in
      ignore (L.build_br loop_bb env.ebuilder);

      (* Loop *)
      build_loop i_addr len "i" loop_bb cont_bb
        (fun i builder ->
          let llargs_i = List.map2
           (fun llarg data_opt -> match data_opt with
             Some(data) -> build_array_load data i "el" builder
           | None -> llarg ) llargs datalist in
          let ret = L.build_call fdef (Array.of_list llargs_i) result builder in
          (match data with 
           Some(d) -> build_array_store d i ret builder |_->()) ; builder ) ;    

     (* Continue *)
     let builder = L.builder_at_end context cont_bb in
     let env = { env with ebuilder = builder } in
     (* Create array struct *)
     (match data with Some(d) ->
     let arr_struct = build_array_struct (ltype_of_typ t) d len
       "arr" env in
     arr_struct, env
     | _ -> l0, env )

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
      (match ret with
        Some(rv) -> (L.build_load rv "if_tmp" env.ebuilder), env
      | None -> (L.const_int (ltype_of_typ Bool) 0), env)

    (* Type casting *)
   | SCast (ex) -> let t_to = t in let (t_from, _) = ex in 
      let normal_cast command = 
        let (lv, env) = expr env ex in
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
      | (Void, _)     -> expr env ex
      | _             -> raise (Failure "Unknown type cast")
      )
   
   in

   (* Builds an SOS statement and returns the updated environment *)
   let build_stmt env = function
     STypeDef(_) -> env (* Everything handled in semant *)
   | SExpression(ex) -> let (_, env) = expr env ex 
     in env
   | SFxnDef(ty, nm, args, ex) ->
     let formal_types =
         Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) args) in
     let ftype = L.function_type (ltype_of_typ ty) formal_types in
     let decl = L.define_function nm ftype the_module in
     let new_builder = L.builder_at_end context (L.entry_block decl) in

     (* Add function to fxns set *)
     let env = add_function env nm decl in

     let new_env = { env with ebuilder=new_builder } in
     let new_env = List.fold_left2 add_formal new_env args (
            Array.to_list (L.params decl)) in
     let new_env = {new_env with ebuilder = new_builder; ecurrent_fxn=decl} in
     let (lv, ret_env) = expr new_env ex in
     
     (* End with a return statement *)
     ( if ty=Void then
     ignore (L.build_ret_void ret_env.ebuilder)
     else
     ignore (L.build_ret lv ret_env.ebuilder) );

     env
   in
  
   (* Build the main function, the entry point for the whole program *)
   let build_main stmts = 
     (* Init the builder at the beginning of main() *)
     let builder = L.builder_at_end context (L.entry_block main) in
     (* Init the starting environment from exeternal functions *)
     let start_env = { ebuilder = builder; evars = StringMap.empty;
       efxns = StringSet.empty; esfxns = StringMap.empty; 
       ecurrent_fxn = main } in
     let start_env = List.fold_left add_external_fxn start_env
       Semant.external_functions in
     (* Build the program *)
     let end_env = List.fold_left build_stmt start_env stmts in
     (* Add a return statement *)
     L.build_ret (L.const_int i32_t 0) end_env.ebuilder    

   in
   ignore(build_main prog);
   the_module
