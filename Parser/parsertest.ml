open Ast

let rec basic_print prog = 
  let rec argtypes_str = function
    [] -> ""
  | (a, b) :: tl -> a ^ " " ^ b ^ ", " ^ argtypes_str tl in
  let print_stmt = function
    Typedef(t) -> let print_tdef = function
      Alias(a, b) -> print_endline ("alias " ^ a ^ " " ^ b)
    | StructDef(a, b) -> print_endline ("struct " ^ a ^ " = {" ^ argtypes_str b ^ "}")
    in print_tdef t
  | Expression(e) -> let rec expr_str = function
      VarDef(a, b, c) -> a ^ " " b ^ " = " ^ expr_str c
    | FxnDef(a, b, c, d) -> a ^ " " ^ b ^ "(" ^ argtypes_str c ^ ") = " expr_str d
    | Assign(a, b) -> a ^ " = " ^ expr_str b
    | Uop(a, b) -> let uoperator_str = function Not -> "!" | Neg -> "-" in uoperator_str a ^ expr_str b
    | Binop(a, b, c) -> let operator_str = function
        Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Mod -> "%"
      | Pow -> "^"
      | Eq -> "="
      | Neq -> "!="
      | Less -> "<"
      | Greater -> ">"
      | LessEq -> "<="
      | GreaterEq -> ">="
      | And -> "&&"
      | Or -> "||"
      | Seq -> ";" in
      "(" ^ expr_str a ^ " " ^ operator_str b ^ " " ^ expr_str c ^ ")"
    | OrderedFxnApp(a, b) -> a ^ "(" ^ List.fold_left (fun x y -> x ^ ", " ^ expr_str y) "" b ^ ")"
    | NamedFxnApp(a, b) -> a ^ "(" ^ List.fold_left (fun x y -> let (a, b) = y in x ^ ", " ^ a ^ ":" expr_str b) "" b ^ ")"
    | IfElse(a, b, c) -> "if " ^ expr_str a ^ " then " ^ expr_str b ^ " else " ^ expr_str c
    | ArrayCon(a) -> "[" ^ List.fold_left (fun x y -> x ^ ", " ^ expr_str y) "" a ^ "]"
    | AnonStruct(a) -> "{" ^ List.fold_left (fun x y -> x ^ ", " ^ expr_str y) "" a ^ "}"
    | NamedStruct(a, b) -> a ^ "{" ^ List.fold_left (fun x y -> x ^ ", " ^ expr_str y) "" b ^ "}"
    | Var(a) -> a
    | StructField(a, b) -> a ^ "." ^ b
    | IntLit(i) -> int_to_string i
    | FloatLit(f) -> float_to_string f
    in print_endline(expr_str e)
  in
  List.map print_stmt prog
