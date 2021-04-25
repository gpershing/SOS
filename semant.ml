(* Semantic checking for the SOS compiler *)

open Ast
open Sast

(* import map for global variables (VarDef, FxnDef, Alias, StructDef) *)
(* we don't have scope defined so a string * tid is enough? *)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each statement *)

(* Environment type for holding all the bindings currently in scope *)
type environment = {
  typemap : typeid StringMap.t;
  fxnnames : StringSet.t;
  varmap : typeid StringMap.t;
}

(* External function signatures *)
(* This is re-used in Codegen *)
(* type, name that to be called in SOS, name in c file *)
let external_functions : (typeid * string) list =
[ Func([Float], Float), "sqrtf" ;
  Func([Float], Float), "sinf" ;
  Func([Float], Float), "cosf" ;
  Func([Float], Float), "tanf" ;
  Func([Float], Float), "asinf" ;
  Func([Float], Float), "acosf" ;
  Func([Float], Float), "atanf" ;
  Func([Float], Float), "toradiansf" ;
  Func([Int; Int], Void), "gl_startRendering" ;
  Func([Int; Int; Int], Void), "gl_endRendering" ;
  Func([Array(Float); Array(Float); Int], Void), "gl_drawCurve" ;
  Func([Array(Float); Array(Float); Int; Int], Void), "gl_drawShape" ;
  Func([Array(Float); Array(Float); Int], Void), "gl_drawPoint" ;
]


let raisestr s = raise (Failure s) 

let check prog =

  (* add built-in function such as basic printing *)
  let built_in_decls =
    let add_bind map (name, ty) = StringMap.add name (Func([ty], Void)) map 
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
                                                 ("printf", Float) ]
  in
  (* add external functions *)
  let built_in_decls = List.fold_left
    (fun map (decl, nm) -> StringMap.add nm decl map)
    built_in_decls external_functions
  in
  let starting_fxns = List.fold_left
    (fun map (_, nm) -> StringSet.add nm map)
    StringSet.empty external_functions
  in
  let starting_fxns = StringSet.add "print" starting_fxns in
  let starting_fxns = StringSet.add "printf" starting_fxns in
(*  (* add math functions *)
  let built_in_decls = List.fold_left
    (fun map (decl, nm) -> StringMap.add nm decl map)
    built_in_decls math_functions
  in *)

  (* add built-in types such as int, float *)
  let built_in_types = (
    let add_type map (name, ty) = StringMap.add name ty map
    in List.fold_left add_type StringMap.empty [("int", Int); ("bool", Bool); ("float", Float); ("void", Void)] )
  in

  (* Initial environment containing built-in types and functions *)
  let global_env = { typemap = built_in_types; varmap = built_in_decls 
  ; fxnnames = starting_fxns }
  in

  (* resolve the type of a tid to a typeid *)
  let rec resolve_typeid t map = match t with
    TypeID(s) -> if StringMap.mem s map
      then StringMap.find s map
      else raisestr ("Could not resolve type id "^s)
  | ArrayTypeID(s) -> Array(resolve_typeid s map)
  | FxnTypeID(l, r) -> Func(List.map (fun tt -> resolve_typeid tt map) l,
          resolve_typeid r map)
  in

  (* should add a function to add three things above dynamically *)
  (* let add_id_type = ()
  in *)

  (* function to lookup *)
  let type_of_id s map = 
    if StringMap.mem s map then StringMap.find s map
    else raisestr ("Unknown variable name "^s)
  in

  (* function to lookup the type of a struct field *)
  let type_of_field stype f = 
    match stype with
      Struct(sargs) -> 
      let rec find_field f = function
        (ft, fn) :: tl -> if fn = f then ft else find_field f tl
        | _ -> raisestr ("Could not find field "^f)
      in find_field f sargs
     | _ -> raisestr ("Cannot access fields for a non-struct variable")
  in

  let add_typedef td map =
    match td with
      Alias(nm, t) -> if StringMap.mem nm map
        then raisestr ("Cannot create an alias with preexisting name " ^ nm)
        else StringMap.add nm (resolve_typeid t map) map
    | StructDef(nm, l) -> let sargl = List.map (fun (t, i) -> (resolve_typeid t map, i)) l in
      StringMap.add nm (Struct(sargl)) map
  in

  let rec add_formals args vmap tmap = match args with
    (typ, nm) :: tl -> add_formals tl (StringMap.add nm (resolve_typeid typ tmap) vmap) tmap
    | _ -> vmap
  in

  (* Matches a struct type component-wise without names *)
  (* Can also work within arrays or other structs *)
  let rec match_str_type t1 t2 =
    match (t1, t2) with
      (Int, Int) -> true
    | (Float, Float) -> true
    | (Bool, Bool) -> true
    | (Void, Void) -> true
    | (Array(a1), Array(a2)) -> match_str_type a1 a2
    | (Struct(s1), Struct(s2)) -> 
        if List.length s1 != List.length s2 then false
        else
        List.fold_left2
          (fun b (st1, _) (st2, _) -> if match_str_type st1 st2 then
             b else false) true s1 s2
    | _ -> false
  in

  (* Returns a sexp that casts sexp to typ, if possible. *)
  (* Returns sexp if no cast is required *)
  let cast_to typ sexp err_str = 
   let (expt, sx) = sexp in
   if expt=typ then sexp else
   if match_str_type expt typ then (typ, sx) else
   if expt=EmptyArray then
    match typ with
      Array(t) -> (Array(t), SArrayCon([]))
    | _ -> raisestr ("Cannot cast empty array to non-array type")
   else
   (
   (match (typ, expt) with
     (Int, Float)   -> ()
   | (Int, Bool)    -> ()
   | (Bool, Int)    -> ()
   | (Bool, Float)  -> ()
   | (Float, Int)   -> ()
   | (Void, _)      -> ()
   | _ -> raisestr err_str );
   (typ, SCast(sexp)))
  in

  (* Function with the same signature as cast_to
   * Used to ignore casting checks *)
  let no_cast typ sexp err_str = 
    ignore(err_str);
    let (expt, _) = sexp in
    if expt=typ then sexp else
    raisestr ("No type casting allowed within arrays")
  in
  
  (* Converts a type to a string *)
  let rec type_str = function
    Int        -> "int"
  | Float      -> "float"
  | Bool       -> "bool"
  | Void       -> "void"
  | Array(t)   -> "array "^type_str t
  | Struct(sl) -> "struct {"^
    (let rec struct_typ_str = function
      (hdt, _) :: (h::t) -> type_str hdt ^ ", " ^struct_typ_str (h::t) 
    | (hdt, _) :: _ -> type_str hdt
    | _ -> ""
    in struct_typ_str sl)^"}"
  | Func(al, rt) -> "func "^
    (let rec func_typ_str = function
      (hdt) :: (h::t) -> type_str hdt ^", "^func_typ_str (h::t)
    | (hdt) :: _ -> type_str hdt
    | _ -> "" in func_typ_str al)^" -> "^type_str rt
  | EmptyArray -> "[]"
  in

  (* Identifies structs with only ints or only floats *)
  let arith_struct t1 at = 
      match t1 with
        Struct(l) -> 
          List.fold_left (fun b (ft, _) -> if ft=at then b else false)
            true l
      | _ -> false
  in
  let either_struct t1 t2 = 
    match t1 with Struct(_) -> true
    | _ -> match t2 with Struct(_) -> true
    | _ -> false
  in
  let is_struct t1 = match t1 with Struct(_) -> true | _ -> false
  in

  let assert_arith t1 = 
    if arith_struct t1 Float then () else
    if arith_struct t1 Int then () else
    raisestr ("Can only operate on arithmetic structs")
  in

  let sop_type t1 =
    if arith_struct t1 Float then Float else
    if arith_struct t1 Int then Int else
    Void
  in

  let addsub_expr env exp1 op exp2 cast = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      (* Can add structs component-wise *)
      if either_struct t1 t2 then
        if match_str_type t1 t2 then
          (assert_arith t1 ;
          (t1, SBinop(exp1, op, exp2)), env)
        else
        raisestr ("Can only add or subtract structs of matching type")

      else
      let err = "Cannot add or subtract "^type_str t1^" and "^type_str t2 in
      match (t1, t2) with
        (Float, _) -> (Float, SBinop(exp1, op, cast Float exp2 err)), env
      | (_, Float) -> (Float, SBinop(cast Float exp1 err, op, exp2)), env
      | (Int, _)   -> (Int,   SBinop(exp1, op, cast Int   exp2 err)), env
      | (_, Int)   -> (Int,   SBinop(cast Int exp1 err, op,   exp2)), env
      | _ -> raisestr (err)
  in

  let mul_expr env exp1 op exp2 cast = 
    let (t1, _) = exp1 in let (t2, _) = exp2 in
    (* Can scale structs and take the dot product *)
    if either_struct t1 t2 then
      if match_str_type t1 t2 then
        (assert_arith t1 ;
        (sop_type t1, SBinop(exp1, op, exp2)), env)

      else if is_struct t1 then
        (assert_arith t1 ;
        (t1, SBinop(exp1, op, cast (sop_type t1) exp2
          "Cannot scale a struct by a non-scalar")), env)
      else
        (assert_arith t2 ;
        (t2, SBinop(exp2, op, cast (sop_type t2) exp1
          "Cannot scale a struct by a non-scalar")), env)
    else
    let err = "Cannot multiply "^type_str t1^" and "^type_str t2 in
    match (t1, t2) with
      (Float, _) -> (Float, SBinop(exp1, op, cast Float exp2 err)), env
    | (_, Float) -> (Float, SBinop(cast Float exp1 err, op, exp2)), env
    | (Int, _)   -> (Int,   SBinop(exp1, op, cast Int   exp2 err)), env
    | (_, Int)   -> (Int,   SBinop(cast Int exp1 err, op,   exp2)), env
    | _ -> raisestr ("Cannot multiply "^type_str t1^" and "^type_str t2)
  in

  let mmul_expr env exp1 op exp2 cast =
    ignore(cast); 
    let (t1, _) = exp1 in let (t2, _) = exp2 in
    (* Can multiply two n * n matrices OR
       Can multiply an n*n matrix with an n*1 vector *)
    match (t1, t2) with
      (Struct(l1), Struct(l2)) ->
      let n1 = List.length l1 in let n2 = List.length l2 in
      let int_sqrt n =
        let rec int_sqrt_inner n m = 
          if m * m = n then Some(m)
          else if m * m < n then int_sqrt_inner n (m+1)
          else None
        in int_sqrt_inner n 1
      in
      let sq1 = int_sqrt n1 in (match sq1 with
        | Some(m1) -> 
          if n1 = n2 then
            (Struct(l1), SBinop(exp1, op, exp2)), env

          else if m1 = n2 then
            (Struct(l2), SBinop(exp1, op, exp2)), env

          else raisestr ("Can only multiply a "^string_of_int m1^" by "^string_of_int m1^" matrix with a square matrix or vector of the same height")
        | None -> raisestr ("Can only multiply square matrices")
      )

    | _ -> raisestr ("Cannot matrix multiply non-structs")
  in

  let div_expr env exp1 op exp2 cast = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      (* Can scale structs *)
      if is_struct t1 then
        (assert_arith t1 ;
        (t1, SBinop(exp1, op, cast (sop_type t1) exp2
          "Cannot scale a struct by a non-scalar")), env)
      else
      let err = "Cannot divide "^type_str t1^" and "^type_str t2 in
      match (t1, t2) with
        (Float, _) -> (Float, SBinop(exp1, op, cast Float exp2 err)), env
      | (_, Float) -> (Float, SBinop(cast Float exp1 err, op, exp2)), env
      | (Int, _)   -> (Int,   SBinop(exp1, op, cast Int   exp2 err)), env
      | (_, Int)   -> (Int,   SBinop(cast Int exp1 err, op,   exp2)), env
      | _ -> raisestr ("Cannot divide "^type_str t1^" and "^type_str t2)
  in

  let mod_expr env exp1 op exp2 cast = 
      ignore (cast); 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      match (t1, t2) with
        (Int, Int) -> (Int, SBinop(exp1, op, exp2)), env
      | _ -> raisestr ("Can only take the modulo with integers")
  in

  let eq_expr env exp1 op exp2 cast = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      (* Can equate arith structs *)
      if either_struct t1 t2 then
        if match_str_type t1 t2 then
          (assert_arith t1 ;
          (Bool, SBinop(exp1, op, exp2)), env)
        else
        raisestr ("Can only equate structs of matching type")

      else
      let err = "Cannot equate "^type_str t1^" and "^type_str t2 in
      match (t1, t2) with
        (Float, _) -> (Bool, SBinop(exp1, op, cast Float exp2 err)), env
      | (_, Float) -> (Bool, SBinop(cast Float exp1 err, op, exp2)), env
      | (Int, _)   -> (Bool, SBinop(exp1, op, cast Int   exp2 err)), env
      | (_, Int)   -> (Bool, SBinop(cast Int exp1 err, op,   exp2)), env
      | _ -> raisestr (err)
  in

  let comp_expr env exp1 op exp2 cast = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      let err = "Cannot compare "^type_str t1^" and "^type_str t2 in
      match (t1, t2) with
        (Float, _) -> (Bool, SBinop(exp1, op, cast Float exp2 err)), env
      | (_, Float) -> (Bool, SBinop(cast Float exp1 err, op, exp2)), env
      | (Int, _)   -> (Bool, SBinop(exp1, op, cast Int exp2 err  )), env
      | (_, Int)  -> (Bool, SBinop(cast Int exp1 err, op, exp2  )), env
      | _ -> raisestr ("Cannot compare "^type_str t1^" and "^type_str t2)
  in

  let logic_expr env exp1 op exp2 cast = 
      let err_str = "Could not resolve boolean operands to boolean values" in
      (Bool, SBinop(cast Bool exp1 err_str, op, cast Bool exp2 err_str)), env
  in

  let array_expr env exp1 op exp2 = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      (match t2 with Array(_) -> ()
       | _ -> raisestr ("Cannot perform array operations on non-array type "^type_str t2)      ) ;
      match op with
        Concat     -> if t1 = t2 then (t2, SBinop(exp1, op, exp2)), env
        else raisestr ("Cannot concatenate arrays of different types")
      | _ (* Of *) -> (t2, SBinop((cast_to Int exp1
                    "First operand of of operator must be an int"),
                  op, exp2)), env
  in

  let rec binop_expr env exp1 op exp2 cast = 
    if op = Of || op = Concat then array_expr env exp1 op exp2
    else
    let e = SVar("empty") in
    let t1, _ = exp1 in
    match t1 with Array(t) ->
      let (ot, _), _ = binop_expr env (t, e) op exp2 no_cast in
      (Array(ot), SBinop(exp1, op, exp2)), env
    | _ ->
    let t2, _ = exp2 in
    match t2 with Array(t) ->
      let (ot, _), _ = binop_expr env exp1 op (t, e) no_cast in
      (Array(ot), SBinop(exp1, op, exp2)), env
    | _ ->
    match op with
      Add -> addsub_expr env exp1 op exp2 cast
    | Sub -> addsub_expr env exp1 op exp2 cast
    | Mul -> mul_expr    env exp1 op exp2 cast
    | MMul-> mmul_expr   env exp1 op exp2 cast
    | Div -> div_expr    env exp1 op exp2 cast
    | Mod -> mod_expr    env exp1 op exp2 cast
    | Eq  -> eq_expr     env exp1 op exp2 cast
    | Neq -> eq_expr     env exp1 op exp2 cast
    | Less      -> comp_expr env exp1 op exp2 cast
    | Greater   -> comp_expr env exp1 op exp2 cast
    | LessEq    -> comp_expr env exp1 op exp2 cast
    | GreaterEq -> comp_expr env exp1 op exp2 cast
    | Or ->  logic_expr env exp1 op exp2 cast
    | And -> logic_expr env exp1 op exp2 cast
    | _ -> raisestr ("Special case, this should never happen")
  in

  (* Takes a pair of sexprs and makes their types agree by adding casts,
   if possible. *)
  let agree_type e1 e2 err_str =
    let ((t1, _), (t2, _)) = (e1, e2) in
    if t1=t2 then (e1, e2) else
    (match (t1, t2) with
     (* Priority is Float -> Int -> Bool *)
      (Void, _)  -> (e1, cast_to t1 e2 err_str)
    | (_, Void)  -> (cast_to t2 e1 err_str, e2)
    | (Float, _) -> (e1, cast_to t1 e2 err_str)
    | (_, Float) -> (cast_to t2 e1 err_str, e2)
    | (Int, _)   -> (e1, cast_to t1 e2 err_str)
    | (_, Int)   -> (cast_to t2 e1 err_str, e2)
    | (Bool, _)  -> (e1, cast_to t1 e2 err_str)
    | (_, Bool)  -> (cast_to t2 e1 err_str, e2)
    | _ -> raisestr err_str )
  in

  let rec assert_nonvoid = function
    Void -> raisestr ("Cannot use a void type in this context")
  | Array(t) -> assert_nonvoid t
  | _ -> ()
  in

  let assert_non_reserved env name =
    if name="copy" || name="free" then
    raisestr ("Cannot create an identifier with reserved name "^name)
    else
    if StringSet.mem name env.fxnnames then
    raisestr ("Cannot create an identifier with defined function name "^name)
    else ()
  in

  let rec expr env = function
     
      VarDef (tstr, name, exp) -> 
        assert_non_reserved env name;
        let (sexp, _) = expr env exp in
        let t = resolve_typeid tstr env.typemap in
        assert_nonvoid t ;
        let (exptype, _) = sexp in
        ((t, SVarDef(t, name, cast_to t sexp
                     ("Could not resolve type when defining "^name^
                      "(Found "^type_str exptype^", expected "^type_str t^")"))),
         { env with varmap = StringMap.add name t env.varmap } )

    | Assign (name, exp) ->
         if StringSet.mem name env.fxnnames then
         raisestr ("Cannot assign a defined (non-variable) function")
         else
         let ((exptype, sexp), _) = expr env exp in
         let t = type_of_id name env.varmap in
         ((t, SAssign(name, cast_to t (exptype, sexp)
                  ("Could not match type when assigning variable "^name^
                  " (Found "^type_str exptype^", expected "^type_str t^")"))),env)

    | AssignStruct (struct_exp, field, exp) ->
         let ((exptype, sexp), env) = expr env exp in
         let (struct_sexp, env)  = expr env struct_exp in 
         let (strt, _) = struct_sexp in
         let t = type_of_field strt field in
         ((t, SAssignStruct(struct_sexp, field, cast_to t (exptype, sexp)
                ("Could not match type when assigning field "^field^
                 " (Found "^type_str exptype^", expected "^type_str t^")"))), env)

    | AssignArray (array_exp, idx, exp) ->
        let ((exptype, sexp), env) = expr env exp in
        let (array_sexp, env) = expr env array_exp in
        let (arrt, _) = array_sexp in
        let (sidx, _) = expr env idx in
        (match sidx with (Int, _) -> () | _ ->
           raisestr ("Array index must be an integer ") );
        let eltype = match arrt with Array(el) -> el | _ -> Void in
        ((eltype, SAssignArray(array_sexp, sidx, cast_to eltype (exptype, sexp)
    ("Could not match type when assigning array "^
    "(Found "^type_str exptype^", expected"^type_str eltype^")"))), env)

    | Uop(op, exp) ->
        let (sexp, env) = expr env exp in (
        match op with
          Not -> (Bool, SUop(op, cast_to Bool sexp 
            "Could not resolve expression to bool")), env
        | Neg -> let (t, _) = sexp in
          (match t with
            Int -> ()
          | Float -> ()
          | _ -> raisestr "Cannot negate non-arithmetic types" );
          (t, SUop(op, sexp)), env )

    | Binop(exp1, op, exp2) -> 
      if op = Seq then
         (* Need to pass new environments *)
         (* jk this happens anyways. but seq still gets to feel special *)
         let (e1, env) = expr env exp1 in
         let (e2, env) = expr env exp2 in
         let (t, _) = e2 in
         (t, SBinop(e1, Seq, e2)), env
      else
         let (e1, env) = expr env exp1 in
         let (e2, env) = expr env exp2 in
         binop_expr env e1 op e2 cast_to

    | FxnApp (exp, args) -> 
         if exp=Var("copy") then (* Copy constructor *)
         (match args with
           [ex] ->
            let (sexp, _) = expr env ex in
            let (t, _) = sexp in
            (match t with
              Array(_) -> ()
            | Struct(_) -> ()
            | _ -> raisestr ("Can only use Copy constructor on reference types"));
             ((t, SFxnApp((Func([t], t), SVar("copy")), [sexp])), env)

         | _ -> raisestr ("Too many arguments for Copy constructor")
         )

         else if exp=Var("free") then (* Free instr *)
         (match args with
           [ex] ->
             let (sexp, _) = expr env ex in
             let (t, _) = sexp in
             (match t with
               Array(_) -> ()
             | Struct(_) -> ()
             | _ -> raisestr ("Can only free memory of struct and array types")) ;
             ((Void, SFxnApp((Func([t], t), SVar("free")), [sexp])), env)
          | _ -> raisestr ("Too many arguments for free()")
          )

         else (* All other functions *)
         let fxn, env = expr env exp in
         let sargs, base_rt = match fxn with (Func(l, t), _) -> l, t
          | _ -> raisestr ("Could not resolve expression to a function") in

         let check_args sigl expl env =
           if (List.length sigl) != (List.length expl) then
           raisestr ("Incorrect number of arguments for function")
           else
           let (l, b) = List.fold_left2 
           (fun (l, arr) typ e ->
            let ((exptype, sexp), _) = expr env e in
            if exptype = Array(typ) then (exptype, sexp) :: l, true
            else (cast_to typ (exptype, sexp) 
                ("Could not match type of argument")) :: l, arr ) 
            ([], false) sigl expl
           in (List.rev l, b)
         in
         let cargs, arrmode = check_args sargs args env in
         if arrmode then
         (( (if base_rt=Void then Void else Array(base_rt)), SIterFxnApp(fxn, cargs)), env)
         else ((base_rt, SFxnApp(fxn, cargs)), env)

    | IfElse (eif, ethen, eelse) -> 
        let (sif, env) = expr env eif in
        let scif = cast_to Bool sif
         "Could not resolve if condition to a bool" in
        let (sthen, _) = expr env ethen in
        let (selse, _)  = expr env eelse in
        let (scthen, scelse) = agree_type sthen selse
          ("Could not reconcile types of then and else clauses ("^
         (let (t,_) = sthen in type_str t)^", "^
         (let (t,_) = selse in type_str t)^")") in
        let (t, _) = scthen in
        ((t, SIfElse(scif, scthen, scelse)), env)

    | ArrayCon l -> (match l with
      hd :: tl ->
        let ((exptype, sexp), env) = expr env hd in 
        assert_nonvoid exptype ;
        let rev_sexprs, env = List.fold_left 
          (fun (l, env) ex -> let (se, env) = expr env ex in
           (cast_to exptype se
             "Could not agree types of array literal") :: l, env)
          ([(exptype, sexp)], env) tl in
        ((Array(exptype), SArrayCon(List.rev rev_sexprs)), env)
      | [] -> ((EmptyArray, SArrayCon([])), env) 
      )

    | AnonStruct l -> 
      let rec create_anon_struct env n = function
        e :: tl -> let ((exptype, sexp), env) = expr env e in
          let (typel, expl), env = create_anon_struct env (n+1) tl in
          ((exptype, "x"^string_of_int n) :: typel, (exptype, sexp) :: expl), env
      | _ -> ([], []), env
      in
      let (typel, expl), env = create_anon_struct env 1 l in
      ((Struct(typel), SStruct("anon", expl)), env)

    | NamedStruct (name, l)  ->
      let st = resolve_typeid (TypeID(name)) env.typemap in
      (match st with
        Struct(sargs) -> 
        let rec create_named_struct env argl = function
          e :: tl -> let (sexp, env) = expr env e in
            (match argl with (t, nm) :: argtl ->
              let stl, env = create_named_struct env argtl tl in
              cast_to t sexp
                ("Could not resolve type of struct field "^nm)
               :: stl, env
            | _ -> raisestr ("Too many arguments for struct "^name))
          | [] -> (match argl with
            [] -> [], env
           | _ -> raisestr ("Not enough arguments for struct "^name))
        in
        let sexprs, env = create_named_struct env sargs l
        in ((st, SStruct(name, sexprs)), env)
      | _ -> raisestr ("Cannot resolve the struct name "^name)
     )

    | Var i -> ((type_of_id i env.varmap, SVar(i)), env)
    | ArrayAccess (arr, idx) -> 
      let (sarr, env) = expr env arr in
      let (sidx, env) = expr env idx in
      let (t, _) = sarr in
      let el_t = (match t with Array(e) -> e
        | _ -> raisestr ("Cannot access elements of non-array variable"))
      in
      ((el_t, SArrayAccess(sarr, cast_to Int sidx
         "Could not cast array index to an integer")), env)
    | StructField (str, fl) -> 
       let (sstr, env) = expr env str in
       let (t, _) = sstr in
       (match t with
         Struct(_) ->
          ((type_of_field t fl, SStructField(sstr ,fl)), env)
       | Array(_) -> if fl="length" then
          (Int, SArrayLength(sstr)), env
          else raisestr ("Cannot access fields for a non-struct variable")
       | _ -> raisestr ("Cannot access fields for a non-struct variable")
      )

    | IntLit i -> ((Int, SIntLit i), env)
    | FloatLit f -> ((Float, SFloatLit f), env)
    | BoolLit b -> ((Bool, SBoolLit b), env)
  in

  let make_stypedef env = function
      Alias(nm, tp) -> SAlias(nm, resolve_typeid tp env.typemap)
    | StructDef(nm , l) -> SStructDef(nm, List.map (fun (t, i) -> let tt = resolve_typeid t env.typemap in assert_nonvoid tt; (tt, i)) l)
  in

  (* check a single statement and update the environment *)
  let stmt env = function
      Expression(e) -> let (se, en) = expr env e in (SExpression (se), en)
    | Typedef(td) -> (STypeDef(make_stypedef env td), {env with typemap = add_typedef td env.typemap})
    | FxnDef (tstr, name, args, exp) ->
        assert_non_reserved env name ;
        let t = resolve_typeid tstr env.typemap in
        let sargs = List.map (fun (tp, nm) -> resolve_typeid tp env.typemap, nm)
          args in
        let argtypes = List.map (fun (tp, _) -> assert_nonvoid tp; tp) sargs in
        let newvarmap = StringMap.add name (Func(argtypes, t)) env.varmap in
        let env = { env with varmap = newvarmap; fxnnames = StringSet.add
            name env.fxnnames } in
        let ((exptype, sx), _) = expr { env with
           varmap = add_formals args env.varmap env.typemap; } exp
        in
        (SFxnDef(t, name, sargs, cast_to t (exptype, sx)
                     ("Incorrect return type for function "^name
                     ^" (Found "^type_str exptype^", expected "^type_str t^")"))),
         env

  in

  let rec stmts env = function 
    hd :: tl -> let (st, en) = stmt env hd in st :: stmts en tl
  | _ -> []
  in stmts global_env prog
