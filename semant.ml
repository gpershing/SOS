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

(* External function signatures *)
(* This is re-used in Codegen *)
type func_decl = func_bind * string
let external_functions : func_decl list =
[ { ftype = Int; formals = [Int, "x"; Int, "y"] }, "example" ]

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
  (* add external functions *)
  let built_in_decls = List.fold_left
    (fun map (decl, nm) -> StringMap.add nm decl map)
    built_in_decls external_functions
  in

  (* add built-in types such as int, float *)
  let built_in_types = (
    let add_type map (name, ty) = StringMap.add name ty map
    in List.fold_left add_type StringMap.empty [("int", Int); ("bool", Bool); ("float", Float); ("void", Void)] )
  in

  (* Initial environment containing built-in types and functions *)
  let global_env = { typemap = built_in_types; varmap = StringMap.empty; funcmap =  built_in_decls }
  in

  (* resolve the type of a tid to a typeid *)
  let rec resolve_typeid t map = match t with
    TypeID(s) -> if StringMap.mem s map
      then StringMap.find s map
      else raisestr ("Could not resolve type id "^s)
  | ArrayTypeID(s) -> Array(resolve_typeid s map)
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
   | _ -> raisestr err_str );
   (typ, SCast(sexp)))
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

  let rec addsub_expr env exp1 op exp2 = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      (* Can add structs component-wise *)
      if either_struct t1 t2 then
        if match_str_type t1 t2 then
          (assert_arith t1 ;
          (t1, SBinop(exp1, op, exp2)), env)
        else
        raisestr ("Can only add or subtract structs of matching type")

      else
      let e = SVar("empty") in
      let err = "Cannot add or subtract "^type_str t1^" and "^type_str t2 in
      match (t1, t2) with
      | (Array(t), _) -> 
        let (ot, _), _ = addsub_expr env (t, e) op exp2 in
        (Array(ot), SBinop(exp1, op, exp2)), env
      | (_, Array(t)) ->
        let (ot, _), _ = addsub_expr env exp1 op (t, e) in
        (Array(ot), SBinop(exp1, op, exp2)), env
      | (Float, _) -> (Float, SBinop(exp1, op, cast_to Float exp2 err)), env
      | (_, Float) -> (Float, SBinop(cast_to Float exp1 err, op, exp2)), env
      | (Int, _)   -> (Int,   SBinop(exp1, op, cast_to Int   exp2 err)), env
      | (_, Int)   -> (Int,   SBinop(cast_to Int exp1 err, op,   exp2)), env
      | _ -> raisestr (err)
  in

  let rec mul_expr env exp1 op exp2 = 
    let (t1, _) = exp1 in let (t2, _) = exp2 in
    (* Can scale structs and take the dot product *)
    if either_struct t1 t2 then
      if match_str_type t1 t2 then
        (assert_arith t1 ;
        (sop_type t1, SBinop(exp1, op, exp2)), env)

      else if is_struct t1 then
        (assert_arith t1 ;
        (t1, SBinop(exp1, op, cast_to (sop_type t1) exp2
          "Cannot scale a struct by a non-scalar")), env)
      else
        (assert_arith t2 ;
        (t2, SBinop(exp2, op, cast_to (sop_type t2) exp1
          "Cannot scale a struct by a non-scalar")), env)
    else
    let e = SVar("empty") in
    let err = "Cannot multiply "^type_str t1^" and "^type_str t2 in
    match (t1, t2) with
      (Array(t), _) ->
      let (ot, _), _ = mul_expr env (t, e) op exp2 in
      (Array(ot), SBinop(exp1, op, exp2)), env
    | (_, Array(t)) ->
      let (ot, _), _ = mul_expr env exp1 op (t, e) in
      (Array(ot), SBinop(exp1, op, exp2)), env
    | (Float, _) -> (Float, SBinop(exp1, op, cast_to Float exp2 err)), env
    | (_, Float) -> (Float, SBinop(cast_to Float exp1 err, op, exp2)), env
    | (Int, _)   -> (Int,   SBinop(exp1, op, cast_to Int   exp2 err)), env
    | (_, Int)   -> (Int,   SBinop(cast_to Int exp1 err, op,   exp2)), env
    | _ -> raisestr ("Cannot multiply "^type_str t1^" and "^type_str t2)
  in

  let mmul_expr env exp1 op exp2 =
    let (t1, _) = exp1 in let (t2, _) = exp2 in
    (* Can multiply two n * n matrices OR
       Can multiply an n*n matrix with an n*1 vector *)
    match t1 with Struct(l1) -> (match t2 with Struct(l2) ->
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

    | _ -> raisestr ("Cannot matrix multiply non-structs") )
    | _ -> raisestr ("Cannot matrix multiply non-structs")
  in

  let rec div_expr env exp1 op exp2 = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      (* Can scale structs *)
      if is_struct t1 then
        (assert_arith t1 ;
        (t1, SBinop(exp1, op, cast_to (sop_type t1) exp2
          "Cannot scale a struct by a non-scalar")), env)
      else
      let e = SVar("empty") in
      let err = "Cannot divide "^type_str t1^" and "^type_str t2 in
      match (t1, t2) with
      (Array(t), _) ->
        let (ot, _), _ = div_expr env (t, e) op exp2 in
        (Array(ot), SBinop(exp1, op, exp2)), env
      | (_, Array(t)) ->
        let (ot, _), _ = div_expr env exp1 op (t, e) in
        (Array(ot), SBinop(exp1, op, exp2)), env
      | (Float, _) -> (Float, SBinop(exp1, op, cast_to Float exp2 err)), env
      | (_, Float) -> (Float, SBinop(cast_to Float exp1 err, op, exp2)), env
      | (Int, _)   -> (Int,   SBinop(exp1, op, cast_to Int   exp2 err)), env
      | (_, Int)   -> (Int,   SBinop(cast_to Int exp1 err, op,   exp2)), env
      | _ -> raisestr ("Cannot divide "^type_str t1^" and "^type_str t2)
  in

  let mod_expr env exp1 op exp2 = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      match (t1, t2) with
        (Int, Int) -> (Int, SBinop(exp1, op, exp2)), env
      | _ -> raisestr ("Can only take the modulo with integers")
  in

  let pow_expr env exp1 op exp2 = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      match (t1, t2) with
        (Int, Int) -> (Int, SBinop(exp1, op, exp2)), env
      | (Float, Int) -> (Float, SBinop(exp1, op, exp2)), env
      | (_, Int) -> raisestr ("Cannot exponentiate "^type_str t1)
      | _ -> raisestr ("Cannot exponentiate to a non-integer power")
  in

  let eq_expr env exp1 op exp2 = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      (match (t1, t2) with
        (Int, Int) -> ()
      | (Float, Float) -> ()
      | _ -> raisestr ("Cannot equate "^type_str t1^" and "^type_str t2) );
      (Bool, SBinop(exp1, op, exp2)), env
  in

  let comp_expr env exp1 op exp2 = 
      let (t1, _) = exp1 in let (t2, _) = exp2 in
      (match (t1, t2) with
        (Int, Int) -> ()
      | (Float, Float) -> ()
      | _ -> raisestr ("Cannot compare "^type_str t1^" and "^type_str t2) );
      (Bool, SBinop(exp1, op, exp2)), env
  in

  let logic_expr env exp1 op exp2 = 
      let err_str = "Could not resolve boolean operands to boolean values" in
      (Bool, SBinop(cast_to Bool exp1 err_str, op, cast_to Bool exp2 err_str)), env
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

  let binop_expr env exp1 op exp2 = match op with
      Add -> addsub_expr env exp1 op exp2
    | Sub -> addsub_expr env exp1 op exp2
    | Mul -> mul_expr    env exp1 op exp2
    | MMul-> mmul_expr   env exp1 op exp2
    | Div -> div_expr    env exp1 op exp2
    | Mod -> mod_expr    env exp1 op exp2
    | Pow -> pow_expr    env exp1 op exp2
    | Eq  -> eq_expr     env exp1 op exp2
    | Neq -> eq_expr     env exp1 op exp2
    | Less      -> comp_expr env exp1 op exp2
    | Greater   -> comp_expr env exp1 op exp2
    | LessEq    -> comp_expr env exp1 op exp2
    | GreaterEq -> comp_expr env exp1 op exp2
    | Or ->  logic_expr env exp1 op exp2
    | And -> logic_expr env exp1 op exp2
    | Of     -> array_expr env exp1 op exp2
    | Concat -> array_expr env exp1 op exp2
    | Seq -> raisestr ("Sequence is a special case, this should never happen")
  in

  (* Takes a pair of sexprs and makes their types agree by adding casts,
   if possible. *)
  let agree_type e1 e2 err_str =
    let ((t1, _), (t2, _)) = (e1, e2) in
    if t1=t2 then (e1, e2) else
    (match (t1, t2) with
     (* Priority is Float -> Int -> Bool *)
      (Float, _) -> (e1, cast_to t1 e2 err_str)
    | (_, Float) -> (cast_to t2 e1 err_str, e2)
    | (Int, _)   -> (e1, cast_to t1 e2 err_str)
    | (_, Int)   -> (cast_to t2 e1 err_str, e2)
    | (Bool, _)  -> (e1, cast_to t1 e2 err_str)
    | (_, Bool)  -> (cast_to t2 e1 err_str, e2)
    | _ -> raisestr err_str )
  in

  let rec expr env = function
     
      VarDef (tstr, name, exp) -> 
        let (sexp, _) = expr env exp in
        let t = resolve_typeid tstr env.typemap in
        let (exptype, _) = sexp in
        ((t, SVarDef(t, name, cast_to t sexp
                     ("Could not resolve type when defining "^name^
                      "(Found "^type_str exptype^", expected "^type_str t^")"))),
         { env with varmap = StringMap.add name t env.varmap } )
          (* TODO may want to deal with overriding variables differently *)

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
        ((t, SFxnDef(t, name, sargs, cast_to t (exptype, sx)
                     ("Incorrect return type for function "^name
                     ^" (Found "^type_str exptype^", expected "^type_str t^")"))),
         { env with funcmap = newfxnmap } )

    | Assign (name, exp) ->
         let ((exptype, sexp), _) = expr env exp in
         let t = type_of_id name env.varmap in
         ((t, SAssign(name, cast_to t (exptype, sexp)
                  ("Could not match type when assigning variable "^name^
                  " (Found "^type_str exptype^", expected "^type_str t^")"))),env)

    | AssignStruct (struct_exp, field, exp) ->
         let ((exptype, sexp), _) = expr env exp in
         let (struct_sexp, _)  = expr env struct_exp in 
         let (strt, _) = struct_sexp in
         let t = type_of_field strt field in
         ((t, SAssignStruct(struct_sexp, field, cast_to t (exptype, sexp)
                ("Could not match type when assigning field "^field^
                 " (Found "^type_str exptype^", expected "^type_str t^")"))), env)

    | AssignArray (array_exp, idx, exp) ->
        let ((exptype, sexp), _) = expr env exp in
        let (array_sexp, _) = expr env array_exp in
        let (arrt, _) = array_sexp in
        let (sidx, _) = expr env idx in
        (match sidx with (Int, _) -> () | _ ->
           raisestr ("Array index must be an integer ") );
        let eltype = match arrt with Array(el) -> el | _ -> Void in
        ((eltype, SAssignArray(array_sexp, sidx, cast_to eltype (exptype, sexp)
    ("Could not match type when assigning array "^
    "(Found "^type_str exptype^", expected"^type_str eltype^")"))), env)

    | Uop(op, exp) ->
        let (sexp, _) = expr env exp in (
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
         let (e1, env) = expr env exp1 in
         let (e2, env) = expr env exp2 in
         let (t, _) = e2 in
         (t, SBinop(e1, Seq, e2)), env
      else
         let (e1, _) = expr env exp1 in
         let (e2, _) = expr env exp2 in
         binop_expr env e1 op e2

    | FxnApp (name, args) -> 
         let check_len args = 
           match args with OrderedFxnArgs(l) -> (List.length l)=1
             | _ -> false
         in
         if name="copy" && check_len args then (* Copy constructor *)
         (match args with
           OrderedFxnArgs([ex]) ->
            let (sexp, _) = expr env ex in
            let (t, _) = sexp in
            (match t with
              Array(_) -> ()
            | Struct(_) -> ()
            | _ -> raisestr ("Can only use Copy constructor on reference types"));
             ((t, SFxnApp(name, SOrderedFxnArgs([sexp]))), env)

         | _ -> raisestr ("Too many arguments for Copy constructor")
         )

         else (* All other functions *)
         let fxn = sig_of_func name env.funcmap in
         (match args with
           OrderedFxnArgs(exps) ->
             let rec check_args sigl expl env = match expl with
               hd :: tl -> let ((exptype, sexp), _) = expr env hd in
                 (match sigl with
                   (typ, nm) :: sigtl ->
                      (cast_to typ (exptype, sexp)
                       ("Could not match type of argument "^nm)) ::
                       check_args sigtl tl env
                   | _ -> raisestr ("Too many arguments for function signature"))
             | [] -> if sigl = [] then [] else raisestr ("Function currying not yet supported")
           in ((fxn.ftype, SFxnApp(name, SOrderedFxnArgs(check_args fxn.formals exps env))), env) 
         | NamedFxnArgs(_) -> raisestr ("Named function arguments not yet supported") (* TODO *)
         )

    | IfElse (eif, ethen, eelse) -> 
        let (sif, _) = expr env eif in
        let scif = cast_to Bool sif
         "Could not resolve if condition to a bool" in
        let (sthen, _) = expr env ethen in
        let (selse, _)  = expr env eelse in
        let (scthen, scelse) = agree_type sthen selse
          "Could not reconcile types of then and else clauses" in
        let (t, _) = scthen in
        ((t, SIfElse(scif, scthen, scelse)), env)

    | ArrayCon l -> (match l with
      hd :: tl ->
        let ((exptype, sexp), _) = expr env hd in 
        ((Array(exptype), SArrayCon((exptype, sexp) :: List.map
          (fun ex -> let (se, _) = expr env ex in
           cast_to exptype se
             "Could not resolve types of array literal"
          )
          tl)), env) (* TODO: Smarter Array type casting *)
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
      ((Struct(typel), SStruct("anon", expl)), env)

    | NamedStruct (name, l)  ->
      let st = resolve_typeid (TypeID(name)) env.typemap in
      (match st with
        Struct(sargs) -> 
        let rec create_named_struct argl = function
          e :: tl -> let (sexp, _) = expr env e in
            (match argl with (t, nm) :: argtl ->
              cast_to t sexp
                ("Could not resolve type of struct field "^nm)
               :: create_named_struct argtl tl
            | _ -> raisestr ("Too many arguments for struct "^name))
          | [] -> (match argl with
            [] -> []
           | _ -> raisestr ("Not enough arguments for struct "^name))
        in
        let sexprs = create_named_struct sargs l
        in ((st, SStruct(name, sexprs)), env)
      | _ -> raisestr ("Cannot resolve the struct name "^name)
     )

    | Var i -> ((type_of_id i env.varmap, SVar(i)), env)
    | ArrayAccess (arr, idx) -> 
      let (sidx, _) = expr env idx in
      let (sarr, _) = expr env arr in
      let (t, _) = sarr in
      let el_t = (match t with Array(e) -> e
        | _ -> raisestr ("Cannot access elements of non-array variable"))
      in
      ((el_t, SArrayAccess(sarr, cast_to Int sidx
         "Could not cast array index to an integer")), env)
    | StructField (str, fl) -> 
       let (sstr, _) = expr env str in
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
    | StructDef(nm , l) -> SStructDef(nm, List.map (fun (t, i) -> (resolve_typeid t env.typemap, i)) l)
  in

  (* check a single statement and update the environment *)
  let stmt env = function
      Expression(e) -> let (se, en) = expr env e in (SExpression (se), en)
    | Typedef(td) -> (STypeDef(make_stypedef env td), {env with typemap = add_typedef td env.typemap})
    | Import(_) -> raisestr ("Import statements not currently supported") (* TODO *)
  in

  let rec stmts env = function 
    hd :: tl -> let (st, en) = stmt env hd in st :: stmts en tl
  | _ -> []
  in stmts global_env prog
