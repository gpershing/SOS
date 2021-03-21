open Ast

let rec comma_list_str f l = match l with
  [] -> ""
| hd :: tl -> match tl with
    [] -> f hd
  | _ -> f hd ^ ", " ^ comma_list_str f tl

let rec typeid_str t = match t with
  TypeID(s) -> s
| ArrayTypeID(p) -> "array " ^ typeid_str p

let rec basic_print prog = 
  let print_stmt = function
    Typedef(t) -> let print_tdef = function
      Alias(a, b) -> print_endline ("alias " ^ typeid_str(a) ^ " " ^ typeid_str(b))
    | StructDef(a, b) -> print_endline ("struct " ^ a ^ " = {" ^ comma_list_str (fun (a, b) -> typeid_str(a) ^ " " ^ b) b ^ "}")
    in print_tdef t
    | Import(f) -> print_endline("import " ^ f)
  | Expression(e) -> let rec expr_str = function
      VarDef(a, b, c) -> typeid_str(a) ^ " " ^ b ^ " = " ^ expr_str c
    | FxnDef(a, b, c, d) -> typeid_str(a) ^ " " ^ b ^ "(" ^ comma_list_str (fun (a, b) -> typeid_str(a) ^ " " ^ b) c ^ ") = " ^ expr_str d
    | Assign(a, b) -> a ^ " = " ^ expr_str b
    | AssignStruct(a, b, c) -> a ^ "." ^ b ^ " = " ^ expr_str c
    | AssignArray(a, b, c) -> a^"["^expr_str b ^"] = "^ expr_str c
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
    | FxnApp(a, c) -> (match c with
        OrderedFxnArgs(b) -> a ^ "(" ^ comma_list_str expr_str b ^ ")"
      | NamedFxnArgs(b) -> a ^ "(" ^ comma_list_str (fun x -> let (a, b) = x in a ^":"^ expr_str b) b ^ ")")
    | IfElse(a, b, c) -> "if " ^ expr_str a ^ " then " ^ expr_str b ^ " else " ^ expr_str c
    | ArrayCon(a) -> "[" ^ comma_list_str expr_str a ^ "]"
    | AnonStruct(a) -> "{" ^ comma_list_str expr_str a ^ "}"
    | NamedStruct(a, b) -> a ^ "{" ^ comma_list_str expr_str b ^ "}"
    | Var(a) -> a
    | StructField(a, b) -> a ^ "." ^ b
    | IntLit(i) -> string_of_int i
    | FloatLit(f) -> string_of_float f
    | BoolLit(true) -> "true"
    | BoolLit(false) -> "false"
    in print_endline(expr_str e)
  in
  List.map print_stmt prog

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prog = Parser.program Scanner.token lexbuf in
  basic_print prog
